package CONV

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._


class OurCONV(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
    override lazy val module = new OurCONVModuleImp(this)
}

class OurCONVModuleImp(outer: OurCONV)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

        val mem = io.mem
        val edge = outer.outerEdge
        

        val sIdle :: sLoad :: sResp :: sDone :: Nil = Enum(4)
        val state = RegInit(sIdle)

        
        val kernelSize = Reg(UInt(2.W))
		val baseAddr = Reg(UInt(32.W))
        val doneSignal = RegInit(0.U(1.W))
        val rd = Reg(UInt(5.W))
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

        mem.a.valid := state === sLoad
        mem.a.bits := edge.Get(
            fromSource = 0.U,
            toAddress = loadAddr,
            lgSize = log2Ceil(32).U)._2
        mem.d.ready := state === sResp

        when (edge.done(mem.a)) {
            printf(p"[ADDR = 0x${Hexadecimal(loadAddr)}]\n")
            state := sResp 
        }

        when (mem.d.fire) {
            kernel(kerRow)(kerCol) := mem.d.bits.data

            when(kerCol === (kernelDim - 1.U)) {
                kerCol := 0.U 
                kerRow := kerRow + 1.U 
            }.otherwise {
                kerCol := kerCol + 1.U 
            }
            state := Mux(loadDone, sDone, sLoad)
            
        }

        when (state === sDone) {
            
            doneSignal := 1.U 
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



