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
        val funct = cmd.bits.inst.funct
        val doWrite = (funct === 1.U) 

        val matrixType = cmd.bits.rs1(0) 
        val targetAddr = cmd.bits.rs2 
        val writeVal = 42.U(xLen.W)

        val reg_rd = Reg(UInt(5.W))
        val reg_xd = Reg(Bool())
        val reg_targetAddr = Reg(UInt(xLen.W))
        val reg_dprv = Reg(UInt(2.W))

        cmd.ready := (state === sIdle)

        // FSM
        when(state === sIdle) {
            when(cmd.fire && doWrite) {
                reg_rd := cmd.bits.inst.rd 
                reg_xd := cmd.bits.inst.xd 
                reg_targetAddr := cmd.bits.rs2 
                reg_dprv := cmd.bits.status.dprv
                state := sWriteReq
                printf("[RoCC] Write command received\n")
            }
        }

        when(state === sWriteReq) {
            val writeData = writeVal
            io.mem.req.valid := true.B 
            io.mem.req.bits.addr := reg_targetAddr
            io.mem.req.bits.tag := 0.U
            io.mem.req.bits.cmd := M_XWR
            io.mem.req.bits.size := log2Ceil(xLen / 8).U
            io.mem.req.bits.signed := false.B
            io.mem.req.bits.data := writeData
            io.mem.req.bits.phys := false.B
            io.mem.req.bits.dprv := reg_dprv

            printf(p"[RoCC] Memory write request for addr: ${io.mem.req.bits.addr} with data: ${io.mem.req.bits.data} sent\n")
            
            when(io.mem.req.ready) {
                state := sDone 
                printf("[RoCC] Memory write request done\n")
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

        // Default
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
	}
