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
		val sIdle :: sLoad :: sResp :: sDone :: Nil = Enum(4)
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
        //val cmd = Queue(io.cmd, 2)
        val funct = io.cmd.bits.inst.funct
        val doLoadKernel = funct === 0.U 

        io.cmd.ready := state === sIdle

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
		
		when (state === sIdle) {
            when(io.cmd.valid) {
                when(doLoadKernel) {
                    // testbench: send done signal
                    rd := io.cmd.bits.inst.rd 
                    printf(p"doLoadKernel\n")
                    kernelSize := io.cmd.bits.rs1
                    baseAddr := io.cmd.bits.rs2
                    printf(p"${io.cmd.bits.rs1}\n")
                    printf(p"${io.cmd.bits.rs2}\n")
                    kerRow := 0.U 
                    kerCol := 0.U 
                    state := sLoad
                }
            }
        }

		io.mem.req.valid := (state === sLoad)
		//io.mem.resp.valid := (state === sResp)

		when (io.mem.req.fire)	 {
			printf(p"Memory request sent: addr = 0x${Hexadecimal(loadAddr)}\n")
			printf(p"memvalid ${io.mem.resp.valid}\n")
			printf(p"memvalid ${io.mem.req.valid}\n")
			printf(p"addr ${io.mem.req.bits.addr}\n")
			printf(p"cmd ${io.mem.req.bits.cmd}\n")
			printf(p"tag ${io.mem.req.bits.tag}\n")
			printf(p"size ${io.mem.req.bits.size}\n")
			printf(p"signed ${io.mem.req.bits.signed}\n")
			printf(p"phys ${io.mem.req.bits.phys}\n")
			
			state := sResp
		}

		when (io.mem.resp.fire) {
			printf(p"Memory response received: addr = 0x${Hexadecimal(loadAddr)}\n")
			kernel(kerRow)(kerCol) := io.mem.resp.bits.data

			when(kerCol === (kernelDim - 1.U)) {
				kerCol := 0.U 
				kerRow := kerRow + 1.U 
			}.otherwise {
				kerCol := kerCol + 1.U 
			}
			state := Mux(loadDone, sDone, sLoad)
		}

		when (state === sLoad) {
			io.mem.req.bits.addr := loadAddr
			io.mem.req.bits.tag := 0.U 
			io.mem.req.bits.cmd := M_XRD// M_XRD
			io.mem.req.bits.size := "b010".U // MT_W (4 bytes)
			io.mem.req.bits.signed := false.B
			io.mem.req.bits.phys := true.B
			printf(p"sLoad\n")
		}

		when (state === sResp) {
			printf(p"sResp\n")
		}

		when (state === sDone) {
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
			when(!io.cmd.valid) {
				doneSignal := 0.U 
				state := sIdle
			}
		}
	}
