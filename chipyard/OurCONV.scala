// chipyard/generators/myaccelerators/src/main/scala
package CONV

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import chisel3.experimental.FixedPoint

class OurCONV(opcodes: OpcodeSet, n: Int = 25)(implicit p: Parameters) extends LazyRoCC(opcodes) {
	val regCount = n
    override lazy val module = new OurCONVModuleImp(this)
}

class OurCONVModuleImp(outer: OurCONV)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
	with HasCoreParameters {
		
		// FSM states
		val sIdle :: sLoadKernel :: sLoadInput :: sDone :: Nil = Enum(4)
		val state = RegInit(sIdle)


		val kernel = Reg(Vec(25, FixedPoint(16.W, 8.BP)))
		val kernelSize = Reg(UInt(2.W)) // Size of the kernel, 0: 1x1, 1: 3x3, 2: 5x5
		

		val input = Reg(Vec(100, FixedPoint(16.W, 8.BP)))
        
		val busy = RegInit(VecInit(Seq.fill(125){false.B}))

        val cmd = Queue(io.cmd)
        val funct = cmd.bits.inst.funct
        val addr = cmd.bits.rs2
        val doWrite = funct === 0.U
        val loadIsInput = funct === 1.U
		val loadIsKernel = funct === 2.U
        val doLoad = funct === 1.U || funct === 2.U
        val doAccum = funct === 3.U
		val doSetKernelSize = funct === 4.U
        val memRespTag = io.mem.resp.bits.tag

        //datapath
        val addend = cmd.bits.rs1
        

		when (cmd.fire && doSetKernelSize) {
			kernelSize := cmd.bits.rs1(1, 0) // Set kernel size based on rs1 bits
			printf(p"Kernel size set to: ${kernelSize}\n")
		}

		when (doLoad && loadIsInput && cmd.fire) {
			state := sLoadInput // Input load
		}
		when (doLoad && loadIsKernel && cmd.fire) {
			state := sLoadKernel // Kernel load
		}

		when (doWrite && cmd.fire) {
			//printf(p"kernel_buffer0 = ${kernel_buffer(0)}\n")
			//printf(p"kernel_buffer1 = ${kernel_buffer(1)}\n")
			//printf(p"kernel_buffer2 = ${kernel_buffer(2)}\n")
			//printf(p"kernel_buffer3 = ${kernel_buffer(3)}\n")
			//printf(p"kernel_buffer4 = ${kernel_buffer(4)}\n")
			//printf(p"kernel_buffer5 = ${kernel_buffer(5)}\n")
			//printf(p"kernel_buffer6 = ${kernel_buffer(6)}\n")
			printf(p"kernel0 = ${kernel(0).asUInt}\n")
			printf(p"kernel1 = ${kernel(1).asUInt}\n")
			printf(p"kernel2 = ${kernel(2).asUInt}\n")
			printf(p"kernel3 = ${kernel(3).asUInt}\n")
			printf(p"kernel4 = ${kernel(4).asUInt}\n")
			printf(p"kernel5 = ${kernel(5).asUInt}\n")
			printf(p"kernel6 = ${kernel(6).asUInt}\n")
			printf(p"kernel7 = ${kernel(7).asUInt}\n")
			printf(p"kernel8 = ${kernel(8).asUInt}\n")
			printf(p"kernel9 = ${kernel(9).asUInt}\n")
			printf(p"kernel10 = ${kernel(10).asUInt}\n")
			printf(p"kernel11 = ${kernel(11).asUInt}\n")
			printf(p"kernel12 = ${kernel(12).asUInt}\n")
			printf(p"kernel13 = ${kernel(13).asUInt}\n")
			printf(p"kernel14 = ${kernel(14).asUInt}\n")
			printf(p"kernel15 = ${kernel(15).asUInt}\n")
			printf(p"kernel16 = ${kernel(16).asUInt}\n")
			printf(p"kernel17 = ${kernel(17).asUInt}\n")
			printf(p"kernel18 = ${kernel(18).asUInt}\n")
			printf(p"kernel19 = ${kernel(19).asUInt}\n")
			printf(p"kernel20 = ${kernel(20).asUInt}\n")
			printf(p"kernel21 = ${kernel(21).asUInt}\n")
			printf(p"kernel22 = ${kernel(22).asUInt}\n")
			printf(p"kernel23 = ${kernel(23).asUInt}\n")
			printf(p"kernel24 = ${kernel(24).asUInt}\n")

			printf(p"input0 = ${input(0).asUInt}\n")
			printf(p"input1 = ${input(1).asUInt}\n")
			printf(p"input2 = ${input(2).asUInt}\n")
			printf(p"input3 = ${input(3).asUInt}\n")
			printf(p"input4 = ${input(4).asUInt}\n")
			printf(p"input5 = ${input(5).asUInt}\n")
			printf(p"input6 = ${input(6).asUInt}\n")
			printf(p"input7 = ${input(7).asUInt}\n")
			printf(p"input8 = ${input(8).asUInt}\n")
			printf(p"input9 = ${input(9).asUInt}\n")
			printf(p"input10 = ${input(10).asUInt}\n")
			printf(p"input11 = ${input(11).asUInt}\n")
			printf(p"input12 = ${input(12).asUInt}\n")
			printf(p"input13 = ${input(13).asUInt}\n")
			printf(p"input14 = ${input(14).asUInt}\n")
			printf(p"input15 = ${input(15).asUInt}\n")
			printf(p"input16 = ${input(16).asUInt}\n")
			printf(p"input17 = ${input(17).asUInt}\n")
			printf(p"input18 = ${input(18).asUInt}\n")
			printf(p"input19 = ${input(19).asUInt}\n")
			printf(p"input20 = ${input(20).asUInt}\n")
			printf(p"input21 = ${input(21).asUInt}\n")
			printf(p"input22 = ${input(22).asUInt}\n")
			printf(p"input23 = ${input(23).asUInt}\n")
			printf(p"input24 = ${input(24).asUInt}\n")

			printf(p"test1 = ${(input(24)*kernel(1)).asUInt}\n")
			
		}

        when (io.mem.resp.valid && state === sLoadKernel) {
			printf(p"MEM RESP: tag=${memRespTag} data=${io.mem.resp.bits.data}\n")
			//kernel_buffer(memRespTag) := io.mem.resp.bits.data
			kernel(memRespTag * 4.U) := io.mem.resp.bits.data(63, 48).asSInt.asFixedPoint(8.BP)
			kernel(memRespTag * 4.U + 1.U) :=io.mem.resp.bits.data(47, 32).asSInt.asFixedPoint(8.BP)
			kernel(memRespTag * 4.U + 2.U) := io.mem.resp.bits.data(31, 16).asSInt.asFixedPoint(8.BP)
			kernel(memRespTag * 4.U + 3.U) := io.mem.resp.bits.data(15, 0).asSInt.asFixedPoint(8.BP)
			busy(memRespTag) := false.B // Optionally clear busy for the first index
			state := sIdle
		}

		when (io.mem.resp.valid && state === sLoadInput) {
			printf(p"MEM RESP: tag=${memRespTag} data=${io.mem.resp.bits.data}\n")
			input(memRespTag * 4.U) := io.mem.resp.bits.data(63, 48).asSInt.asFixedPoint(8.BP)
			input(memRespTag * 4.U + 1.U) :=io.mem.resp.bits.data(47, 32).asSInt.asFixedPoint(8.BP)
			input(memRespTag * 4.U + 2.U) := io.mem.resp.bits.data(31, 16).asSInt.asFixedPoint(8.BP)
			input(memRespTag * 4.U + 3.U) := io.mem.resp.bits.data(15, 0).asSInt.asFixedPoint(8.BP)
			busy(memRespTag + 25.U) := false.B // Optionally clear busy for the first index
			state := sIdle
		}

        // control
        when (io.mem.req.fire) {
			when (state === sLoadKernel) {
            	busy(addr) := true.B
			}
			when (state === sLoadInput) {
				busy(addr+25.U) := true.B
			}
        }

        val doResp = cmd.bits.inst.xd
        val stallReg = busy(addr)
        val stallLoad = doLoad && !io.mem.req.ready
        val stallResp = doResp && !io.resp.ready

        cmd.ready := !stallReg && !stallLoad && !stallResp

        // PROC RESPONSE INTERFACE
        io.resp.valid := cmd.valid && doResp && !stallReg && !stallLoad
        io.resp.bits.rd := cmd.bits.inst.rd
        io.resp.bits.data := 1.U

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
