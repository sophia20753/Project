// chipyard/generators/myaccelerators/src/main/scala
package CONV

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._

class OurCONV(opcodes: OpcodeSet, n: Int = 8)(implicit p: Parameters) extends LazyRoCC(opcodes) {
    override lazy val module = new OurCONVModuleImp(this)
}

class OurCONVModuleImp(outer: OurCONV)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
	with HasCoreParameters {
		// FSM states
        val sIdle :: sWriteReq :: sWaitResp :: sDone :: Nil = Enum(4)
        val state = RegInit(sIdle)

        val cmd = Queue(io.cmd)
        val funct = cmd.bits.inst.funct // funct 7
        val doWrite = (funct === 1.U) 

        val stride = 2.U // 16-bit stride
        val writeIdx = RegInit(0.U(8.W))
        val writeVal = 42.U(xLen.W)

        val reg_rd = Reg(UInt(5.W))
        val reg_xd = Reg(Bool())
        val reg_baseAddr = Reg(UInt(xLen.W))
        val reg_dprv = Reg(UInt(2.W))
        val reg_numElements = Reg(UInt(8.W))

        // Packing logic
        val elementsPerWord = 4.U 
        val totalPackedWrites = (reg_numElements + 3.U) >> 2 

        // Default
        cmd.ready := (state === sIdle)

        when(state =/= sWriteReq) {
            io.mem.req.valid := false.B 
            io.mem.req.bits.addr := 0.U
            io.mem.req.bits.tag := 0.U
            io.mem.req.bits.cmd := M_XWR
            io.mem.req.bits.size := log2Ceil(xLen / 8).U
            io.mem.req.bits.signed := false.B
            io.mem.req.bits.data := 0.U
            io.mem.req.bits.phys := false.B
            io.mem.req.bits.dprv := reg_dprv
        }

        when(state =/= sDone) {
            io.resp.valid := false.B 
            io.resp.bits := DontCare
        }

        io.busy := (state =/= sIdle)
        io.interrupt := false.B

        // FSM
        when(state === sIdle) {
            when(cmd.fire && doWrite) {
                reg_rd := cmd.bits.inst.rd 
                reg_xd := cmd.bits.inst.xd 
                reg_baseAddr := cmd.bits.rs2 
                reg_dprv := cmd.bits.status.dprv
                reg_numElements := cmd.bits.rs1
                writeIdx := 0.U
                state := sWriteReq
                printf("[RoCC] Write command received\n")
            }
        }

        when(state === sWriteReq) {
            when(writeIdx < reg_numElements) {
                // Packing logic
                val base = writeIdx << 2 
                val v0 = (writeVal + base)(15,0)
                val v1 = (writeVal + base + 1.U)(15,0)
                val v2 = (writeVal + base + 2.U)(15,0)
                val v3 = (writeVal + base + 3.U)(15,0)
                val packedData = Cat(v3, v2, v1, v0)

                io.mem.req.valid := true.B 
                io.mem.req.bits.addr := reg_baseAddr + (writeIdx << 3)
                io.mem.req.bits.tag := writeIdx + 1.U // must be non-zero
                io.mem.req.bits.cmd := M_XWR
                io.mem.req.bits.size := 3.U // 64 bits
                io.mem.req.bits.signed := false.B 
                io.mem.req.bits.data := packedData
                io.mem.req.bits.phys := false.B 
                io.mem.req.bits.dprv := reg_dprv

                when(io.mem.req.fire) {
                    printf(p"[RoCC] Sent write: addr=0x${Hexadecimal(io.mem.req.bits.addr)}, tag=${io.mem.req.bits.tag}, data=0x${Hexadecimal(io.mem.req.bits.data)}\n")
                    writeIdx := writeIdx + 1.U 
                    when(writeIdx + 1.U === reg_numElements) {
                        state := sWaitResp 
                        printf("[RoCC] All write requests issued, waiting for responses\n")
                    }
                }
            }
        }

        when(state === sWaitResp) {
            when(io.mem.resp.valid) {
                val tag = io.mem.resp.bits.tag 
                when(tag === reg_numElements) {
                    state := sDone
                    printf("[RoCC] All write responses received\n")
                }
            }
        }
        
        when(state === sDone) {
            when(reg_xd && io.resp.ready) {
                io.resp.valid := true.B 
                io.resp.bits.rd := reg_rd 
                io.resp.bits.data := 1.U 
                state := sIdle
                printf(p"[RoCC] Written data: ${io.resp.bits.data} to rd: ${io.resp.bits.rd} success back\n")
            }.elsewhen(!reg_xd) {
                state := sIdle
            }
        }
	}
