// chipyard/generators/myaccelerators/src/main/scala
package CONV

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._

class OurCONV(opcodes: OpcodeSet, n: Int = 8)(implicit p: Parameters) extends LazyRoCC(opcodes) {
	val regCount = n
    override lazy val module = new OurCONVModuleImp(this)
}

class OurCONVModuleImp(outer: OurCONV)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
	with HasCoreParameters {
		// FSM states
		val regfile = Mem(outer.regCount, UInt(xLen.W))
        val busy = RegInit(VecInit(Seq.fill(outer.regCount){false.B}))

        val cmd = Queue(io.cmd)
        val funct = cmd.bits.inst.funct
        val addr = cmd.bits.rs2(log2Up(outer.regCount)-1,0)
        val doWrite = funct === 0.U
        val doRead = funct === 1.U
        val doLoad = funct === 2.U
        val doAccum = funct === 3.U
        val memRespTag = io.mem.resp.bits.tag(log2Up(outer.regCount)-1,0)

        //datapath
        val addend = cmd.bits.rs1
        val accum = regfile(addr)
        val wdata = Mux(doWrite, addend, accum + addend)

        when (cmd.fire && (doWrite || doAccum)) {
            regfile(addr) := wdata
        }

        when (io.mem.resp.valid) {
            regfile(memRespTag) := io.mem.resp.bits.data
            busy(memRespTag) := false.B
        }

        // control
        when (io.mem.req.fire) {
            busy(addr) := true.B
        }

        val doResp = cmd.bits.inst.xd
        val stallReg = busy(addr)
        val stallLoad = doLoad && !io.mem.req.ready
        val stallResp = doResp && !io.resp.ready

        cmd.ready := !stallReg && !stallLoad && !stallResp

        // PROC RESPONSE INTERFACE
        io.resp.valid := cmd.valid && doResp && !stallReg && !stallLoad
        io.resp.bits.rd := cmd.bits.inst.rd
        io.resp.bits.data := accum

        io.busy := cmd.valid || busy.reduce(_ || _)
        io.interrupt := false.B
        

        // Memory request interface
        io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
        io.mem.req.bits.addr := addend
        io.mem.req.bits.tag := addr
        io.mem.req.bits.cmd := M_XRD
        io.mem.req.bits.size := log2Ceil(8).U
        io.mem.req.bits.signed := false.B
        io.mem.req.bits.data := 0.U
        io.mem.req.bits.phys := false.B
        io.mem.req.bits.dprv := cmd.bits.status.dprv

	}
