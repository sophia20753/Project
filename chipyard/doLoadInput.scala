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
        val sIdle :: sReadTile :: sCompute :: sDone :: Nil = Enum(4)
        val state = RegInit(sIdle)

        val cmd = Queue(io.cmd)
        val funct = cmd.bits.inst.funct
        val doLoadInput = (funct === 1.U)

        val currInputTile = Reg(Vec(64, UInt(16.W))) // current 8x8 tile for computation
        val tileIndex = RegInit(0.U(4.W)) // 0 to 15 for 16 tiles 

        val readReq = RegInit(0.U(5.W)) // 0 to 15 (16 reqs per tile)
        val readResp = RegInit(0.U(5.W))
        
        val reg_inputBaseAddr = Reg(UInt(xLen.W))
        val reg_tileNum = Reg(UInt(4.W)) // for testbench
        val reg_rd = Reg(UInt(5.W))
        val reg_xd = Reg(Bool())
        val reg_dprv = Reg(UInt(2.W))

        val tileNum = reg_tileNum

        // Default
        cmd.ready := (state === sIdle) 

        when(state =/= sReadTile) {
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

        // Calculate tile base address
        val tileRow = tileIndex / 4.U 
        val tileCol = tileIndex % 4.U 
        val tileBaseOffset = ((tileRow * 32.U + tileCol * 8.U) * 2.U) // 2 bytes per 16-bit element
        val tileAddr = reg_inputBaseAddr + tileBaseOffset

        switch(state) {
            is(sIdle) {
                when(cmd.fire && doLoadInput) {
                    reg_rd := cmd.bits.inst.rd 
                    reg_xd := cmd.bits.inst.xd 
                    reg_dprv := cmd.bits.status.dprv 
                    reg_inputBaseAddr := cmd.bits.rs1 
                    reg_tileNum := cmd.bits.rs2 // for testbench
                    tileIndex := 0.U
                    readReq := 0.U 
                    readResp := 0.U

                    state := sReadTile
                    printf("[RoCC] Read command received\n")
                }
            }
            is(sReadTile) {
                val canIssue = readReq < 16.U 
                val canResp = readResp < 16.U 
                io.mem.req.valid := canIssue
                io.mem.req.bits.addr := tileAddr + (readReq << 3)
                io.mem.req.bits.tag := readReq + 1.U 
                io.mem.req.bits.cmd := M_XRD
                io.mem.req.bits.size := log2Ceil(xLen/8).U 
                io.mem.req.bits.signed := false.B 
                io.mem.req.bits.data := 0.U 
                io.mem.req.bits.phys := false.B 
                io.mem.req.bits.dprv := reg_dprv 

                when(io.mem.req.fire) {
                    readReq := readReq + 1.U 
                    printf(p"[RoCC] Sent tile read: tileIndex=${tileIndex}, addr=0x${Hexadecimal(io.mem.req.bits.addr)}, tag=${io.mem.req.bits.tag}\n")
                }

                when(io.mem.resp.valid && canResp) {
                    printf(p"[RoCC] Responded tile read: tileIndex=${tileIndex}, tag=${io.mem.resp.bits.tag}, data=0x${Hexadecimal(io.mem.resp.bits.data)}\n")    
                    val tag = io.mem.resp.bits.tag 
                    val data = io.mem.resp.bits.data 
                    readResp := readResp + 1.U 

                    // Unpack data into input vector
                    for (i <- 0 until 4) {
                        val idx = ((tag - 1.U) << 2) + i.U 
                        when(idx < 64.U) {
                            val v = (data >> (16*i)).asUInt()(15,0)
                            currInputTile(idx) := v
                            printf("[RoCC] Tile[%d] = 0x%x\n", idx, v)
                        }
                    }
                }

                when(!canIssue && !canResp) { // should read 16 words (16 * 4 = 64 elements)
                    state := sCompute
                    printf("[RoCC] All elements loaded for tile %d\n", tileIndex)
                }
            }
            is(sCompute) {
                printf("[RoCC] Finished loading tile %d, computing here\n", tileIndex)

                // COMPUTE

                when(tileIndex === tileNum) {
                    state := sDone
                }.otherwise {
                    tileIndex := tileIndex + 1.U 
                    readReq := 0.U 
                    readResp := 0.U 
                    state := sReadTile
                }
            }
            is(sDone) {
                when(reg_xd && io.resp.ready) {
                    io.resp.valid := true.B 
                    io.resp.bits.rd := reg_rd 
                    io.resp.bits.data := 1.U 
                    state := sIdle 
                    printf("[RoCC] All tiles processed. Returning.\n")
                }.elsewhen(!reg_xd) {
                    // Something went wrong
                    state := sIdle
                }
            }
        }
    }