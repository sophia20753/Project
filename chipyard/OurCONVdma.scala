// chipyard/generators/myaccelerators/src/main/scala
package CONV

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._

class OurCONV(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
	override lazy val module = new OurCONVModuleImp(this)
}

class simpleDMA(val maxLen: Int = 1024)(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
        val start       = Input(Bool())             // Trigger signal from CONV
        val baseAddr    = Input(UInt(32.W))         // Starting address of data
        val length      = Input(UInt(32.W))         // total size in bytes
        val done        = Output(Bool())            // Done signal for CONV

        val mem         = new HellaCacheIO          
        val dataOut     = Decoupled(UInt(32.W))     // Stream of 32-bit values
    })

    val sIdle :: sRead :: sWait :: sDone :: Nil = Enum(4)
    val state = RegInit(sIdle)

    val offset = RegInit(0.U(32.W))     // Track read progress
    val totalBytes = Reg(UInt(32.W))    // Total bytes to read
    val addr = io.baseAddr + offset     // Next address

    io.done := (state === sDone)
    
	io.mem.req.valid := false.B
	io.mem.req.bits.addr := 0.U
	io.mem.req.bits.tag := 0.U
	io.mem.req.bits.cmd := 0.U // e.g., M_XRD
	io.mem.req.bits.size := 0.U
	io.mem.req.bits.signed := false.B
	io.mem.req.bits.data := 0.U
	io.mem.req.bits.phys := false.B
	io.mem.req.bits.no_alloc := false.B
	io.mem.req.bits.no_xcpt := false.B
	io.mem.req.bits.mask := 0.U
	io.mem.req.bits.dprv := 0.U
	io.mem.req.bits.dv := false.B

	io.mem.s1_kill := false.B
	io.mem.s1_data.mask := 0.U
	io.mem.s1_data.data := 0.U
	io.mem.s2_kill := false.B
	io.mem.keep_clock_enabled := false.B
	
    io.dataOut.valid := false.B 
    io.dataOut.bits := DontCare
    
    printf("[DMA] simpleDMA module initialised. State : %d\n", state)

    when(io.start && state === sIdle) {
        totalBytes := io.length 
        offset := 0.U 
        state := sRead
        printf("[DMA] Start signal received. Total bytes: %d\n", io.length)
    }

    when(state === sRead) {
        // Prepare memory request
		io.mem.req.valid := true.B
		io.mem.req.bits.addr   := addr
		io.mem.req.bits.tag    := 0.U
		io.mem.req.bits.cmd    := M_XRD
		io.mem.req.bits.size   := "b010".U       // 4 bytes = 32 bits
		io.mem.req.bits.signed := false.B
		io.mem.req.bits.data   := 0.U
		io.mem.req.bits.dprv   := 0.U            // Fill required fields
		io.mem.req.bits.dv     := false.B
		io.mem.req.bits.phys   := false.B
		io.mem.req.bits.mask   := "b1111".U      // 4-byte mask

		printf("[DMA] Sending read req. Addr: 0x%x, Offset: %d\n", addr, offset)
        when(io.mem.req.fire) {
        	printf("[DMA] Memory request fired.\n")
            state := sWait
        }
    }

    when(state === sWait) {
        when(io.mem.resp.valid) {
            io.dataOut.valid := true.B 
            io.dataOut.bits := io.mem.resp.bits.data 
            printf("[DMA] Received data: 0x%x\n", io.mem.resp.bits.data)
            when(io.dataOut.ready) {
                offset := offset + 4.U 
                printf("[DMA] Data sent to output. Next offset: %d\n", offset + 4.U)
                when(offset + 4.U >= totalBytes) {
                	printf("[DMA] All data read. Moving to done state.\n")
                    state := sDone 
                }.otherwise{
                    state := sRead
                }
            }
        }
    }
    
    when(state === sDone) {
        when(!io.start) {
        	printf("[DMA] Resetting to idle state.\n")
            state := sIdle
        }
    }
}

class OurCONVModuleImp(outer: OurCONV)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
	with HasCoreParameters {
		// FSM states
		val sIdle :: sLoad :: sWaitMem :: sDone :: Nil = Enum(4)
		val state = RegInit(sIdle)

		// Internal registers
		val doneSignal = RegInit(0.U(1.W))
		val rd = Reg(UInt(5.W))
		val kernelSize = Reg(UInt(2.W))
		val baseAddr = Reg(UInt(32.W))

		val kernel = Reg(Vec(5, Vec(5, UInt(32.W))))
		val loadCounter = RegInit(0.U(5.W))

        // cmd signals
        val cmd = Queue(io.cmd, 2)
        val funct = cmd.bits.inst.funct
        val doLoadKernel = funct === 0.U 

        cmd.ready := state === sIdle

		io.resp.valid := (state === sDone)
		io.resp.bits.rd := rd
		io.resp.bits.data := doneSignal

		// Decode size
		val kernelDim = Wire(UInt(3.W))
		kernelDim := MuxLookup(kernelSize, 1.U, Seq(
		    0.U -> 1.U,
		    1.U -> 3.U,
		    2.U -> 5.U
		))
		val totalKernelElements = kernelDim * kernelDim 

		// Instantiate DMA
		val dma = Module(new simpleDMA())
		dma.io.start := (state === sLoad)
		dma.io.baseAddr := baseAddr 
		dma.io.length := totalKernelElements << 2 

		// Connect memory interface to RoCC
		io.mem <> dma.io.mem 

		dma.io.dataOut.ready := (state === sWaitMem)

		switch(state) {
		    is(sIdle) {
		        when(cmd.valid && doLoadKernel) {
					// testbench: send done signal
					rd := cmd.bits.inst.rd 
					printf(p"doLoadKernel")
					kernelSize := cmd.bits.rs1
					baseAddr := cmd.bits.rs2
					loadCounter := 0.U
					state := sLoad
		        }
		    }

		    is(sLoad) {
				printf(p"sLoad")
		        when(dma.io.done) {
		            state := sWaitMem 
		        }
		    }
		    // TODO: DMA bulk memory transfer
		    is(sWaitMem) {
		        when(dma.io.dataOut.valid && dma.io.dataOut.ready) {
					val row = loadCounter / kernelDim
					val col = loadCounter % kernelDim
					kernel(row)(col) := dma.io.dataOut.bits 
					loadCounter := loadCounter + 1.U 
					when(loadCounter + 1.U === totalKernelElements) {
						state := sDone
					}
				}
		    }

		    is(sDone) {
				// testbench: send done signal
				doneSignal := 1.U 
				// testbench: print loaded kernel
				printf(p"[RoCC Kernel loaded:\n]")
				for (i <- 0 until 5) {
					for (j <- 0 until 5) {
						printf(p"${kernel(i)(j)} ")
					}
					printf(p"\n")
				}
		        when(!cmd.valid) {
					doneSignal := 0.U 
		            state := sIdle
		        }
		    }
		}
}