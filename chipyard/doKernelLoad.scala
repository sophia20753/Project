// chipyard/generators/myaccelerators/src/main/scala
package CONV

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import chisel3.experimental.FixedPoint

class OurCONV(opcodes: OpcodeSet, n: Int = 8)(implicit p: Parameters) extends LazyRoCC(opcodes) {
    override lazy val module = new OurCONVModuleImp(this)
}

class OurCONVModuleImp(outer: OurCONV)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
	with HasCoreParameters {
        // FSM states
        val sIdle :: sReadReq :: sWaitResp :: sDone :: Nil = Enum(4)
        val state = RegInit(sIdle) 
        
        val cmd = Queue(io.cmd) 
        val funct = cmd.bits.inst.funct
        val doLoadKernel = (funct === 2.U)

        val kernel = Reg(Vec(5, Vec(5, UInt(16.W))))
        // val kernel = Reg(Vec(25, UInt(16.W)))
        val kerRow = RegInit(0.U(3.W))
        val kerCol = RegInit(0.U(3.W))

        val reg_kernelSize = Reg(UInt(2.W))
        val reg_kernelBaseAddr = Reg(UInt(xLen.W))

        val reg_kernelDim = RegInit(0.U(3.W))

        val kernelNumElements = reg_kernelDim * reg_kernelDim

        val readReq = RegInit(0.U(8.W)) // tracks number of packed 64-bit words read requests sent
        val readResp = RegInit(0.U(8.W)) // tracks number of packed 64-bit words read requests responded 
        val totalReadReq = ((kernelNumElements + 3.U) >> 2).asUInt

        val reg_rd = Reg(UInt(5.W))
        val reg_xd = Reg(Bool())
        val reg_dprv = Reg(UInt(2.W))

        // Default
        cmd.ready := (state === sIdle) 

        when(state =/= sReadReq) {
            io.mem.req.valid := false.B 
            io.mem.req.bits.addr := 0.U
            io.mem.req.bits.tag := 0.U
            io.mem.req.bits.cmd := M_XRD
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
        switch(state) {
            is(sIdle) {
                when(cmd.fire && doLoadKernel) {
                    reg_rd := cmd.bits.inst.rd 
                    reg_xd := cmd.bits.inst.xd 
                    reg_dprv := cmd.bits.status.dprv 
                    reg_kernelBaseAddr := cmd.bits.rs1 
                    reg_kernelDim := cmd.bits.rs2 
                    readReq := 0.U 
                    readResp := 0.U

                    state := sReadReq
                    printf("[RoCC] Read command received\n")
                }
            }
            is(sReadReq) {
                // Issue request
                val canIssue = readReq < totalReadReq
                val canResp = readResp < totalReadReq
                io.mem.req.valid := canIssue 
                io.mem.req.bits.addr := reg_kernelBaseAddr + (readReq << 3)
                io.mem.req.bits.tag := readReq + 1.U // must be non-zero 
                io.mem.req.bits.cmd := M_XRD 
                io.mem.req.bits.size := log2Ceil(xLen/8).U 
                io.mem.req.bits.signed := false.B // change this
                io.mem.req.bits.data := 0.U // only for writes
                io.mem.req.bits.phys := false.B 
                io.mem.req.bits.dprv := reg_dprv 
                
                when(io.mem.req.fire) {
                    readReq := readReq + 1.U 
                    printf(p"[RoCC] Sent kernel read: addr=0x${Hexadecimal(io.mem.req.bits.addr)}, tag=${io.mem.req.bits.tag}\n")
                }

                // Handle response
                when(io.mem.resp.valid && canResp) {   
                    printf(p"[RoCC] Responded kernel read: tag=${io.mem.resp.bits.tag}, data=0x${Hexadecimal(io.mem.resp.bits.data)}\n")          
                    val tag = io.mem.resp.bits.tag 
                    val data = io.mem.resp.bits.data 

                    readResp := readResp + 1.U 

                    // Unpack data into kernel vector
                    for (i <- 0 until 4) {
                        val flatIdx = ((tag - 1.U) << 2) + i.U 
                        when(flatIdx < kernelNumElements) {
                            val row = flatIdx / reg_kernelDim 
                            val col = flatIdx % reg_kernelDim 
                            val v = (data >> (16*i)).asUInt()(15, 0)
                            kernel(row)(col) := v
                            printf("[RoCC] Wrote kernel(%d)(%d) = 0x%x\n", row, col, kernel(row)(col))
                        }
                    }

                    // val baseIdx = (tag - 1.U) << 2
                    // for (i <- 0 until 4) {
                    //     val flatIdx = baseIdx + i.U 
                    //     when(flatIdx < kernelNumElements) {
                    //         kernel(flatIdx) := (data >> (16*i)).asUInt()(15, 0)
                    //         printf("[RoCC] kernel[%d] = 0x%x\n", flatIdx, (data >> (16 * i))(15, 0))
                    //     }
                    // }
                    // readResp := readResp + 1.U
                }

                // Check for all requests issued, all requests responded
                when(!canIssue && !canResp) {
                    state := sDone 
                    printf("[RoCC] All kernel words loaded")
                }
            }
            is(sDone) {
                when(reg_xd && io.resp.ready) {
                    io.resp.valid := true.B 
                    io.resp.bits.rd := reg_rd 
                    io.resp.bits.data := 1.U 
                    state := sIdle 
                    printf("[RoCC] Written data: %x to rd: %d success back\n", io.resp.bits.data, io.resp.bits.rd)
                }.elsewhen(!reg_xd) {
                    // Something went wrong
                    state := sIdle
                }
            }
        }
    }