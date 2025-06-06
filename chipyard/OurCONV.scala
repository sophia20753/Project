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

		val kerRow = RegInit(0.U(3.W))
		val kerCol = RegInit(0.U(3.W))
		val kernel = Reg(Vec(5, Vec(5, UInt(32.W))))

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

		val index = kerRow * kernelDim + kerCol
		val byteOffset = index << 2 // index * 4 (for word addressing)
		val loadAddr = baseAddr + byteOffset

		val loadDone = (kerRow === (kernelDim - 1.U)) && (kerCol === (kernelDim - 1.U)) // possible timing condition

		io.mem.req.valid := (state === sLoad)
		when(state === sLoad) {
			io.mem.req.bits.addr := loadAddr
			io.mem.req.bits.tag := 0.U 
			io.mem.req.bits.cmd := "b000".U // M_XRD
			io.mem.req.bits.size := "b010".U // MT_W (4 bytes)
			io.mem.req.bits.signed := false.B
			io.mem.req.bits.data := 0.U 
		}

		switch(state) {
		    is(sIdle) {
		        when(cmd.valid) {
                    when(doLoadKernel) {
						// testbench: send done signal
						rd := cmd.bits.inst.rd 
						printf(p"doLoadKernel")
                        kernelSize := cmd.bits.rs1
                        baseAddr := cmd.bits.rs2
                        kerRow := 0.U 
                        kerCol := 0.U 
                        state := sLoad
                    }
		        }
		    }

		    is(sLoad) {
				printf(p"sLoad")
		        when(io.mem.req.fire) {
		            state := sWaitMem 
		        }
		    }
		    // TODO: DMA bulk memory transfer
		    is(sWaitMem) {
		        when(io.mem.resp.fire) {
					printf(p"[ADDR = 0x${Hexadecimal(loadAddr)}]\n")
		            kernel(kerRow)(kerCol) := io.mem.resp.bits.data

		            when(kerCol === (kernelDim - 1.U)) {
		                kerCol := 0.U 
		                kerRow := kerRow + 1.U 
		            }.otherwise {
		                kerCol := kerCol + 1.U 
		            }
		            state := Mux(loadDone, sDone, sLoad)
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

// DMA
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
		val dma = Module(new DMA())
		dma.io.start := (state === sLoad)
		dma.io.baseAddr := baseAddr 
		dma.io.length := totalKernelElements << 2 

		// Connect memory interface to RoCC
		io.mem <> dma.io.mem 

		dma.io.dataOut.ready := (state === sWaitMem)

		when(state === sLoad) {
			io.mem.req.bits.addr := loadAddr
			io.mem.req.bits.tag := 0.U 
			io.mem.req.bits.cmd := "b000".U // M_XRD
			io.mem.req.bits.size := "b010".U // MT_W (4 bytes)
			io.mem.req.bits.signed := false.B
			io.mem.req.bits.data := 0.U 
		}

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