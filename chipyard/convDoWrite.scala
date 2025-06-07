// chipyard/generators/myaccelerators/src/main/scala
package CONV

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import chisel3.experimental.FixedPoint
import scala.math._

class OurCONV(opcodes: OpcodeSet, n: Int = 8)(implicit p: Parameters) extends LazyRoCC(opcodes) {
    override lazy val module = new OurCONVModuleImp(this)
}

class OurCONVModuleImp(outer: OurCONV)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
	with HasCoreParameters {
        val N = 2
        val K = 1
        val pad = (K - 1) / 2

        val sIdle :: sSetup ::sLoad :: sAcc1 :: sAcc2 :: writeResult ::sWriteReq :: sWaitResp :: sDone :: Nil = Enum(9)
        val topLeft :: top :: topRight :: left :: center :: right :: bottomLeft :: bottom :: bottomRight :: full :: Nil = Enum(10)
        val state = RegInit(sIdle)

        val cmd = Queue(io.cmd)
        val funct = cmd.bits.inst.funct // funct 7
        val doCompute = (funct === 3.U)
        
        val kernel = VecInit(Seq(
            VecInit(Seq(128.0.F(16.W, 8.BP)))
        ))

        val input = VecInit(Seq(
            VecInit(Seq(1.0.F(16.W, 8.BP), 2.0.F(16.W, 8.BP))),
            VecInit(Seq(3.0.F(16.W, 8.BP), 4.0.F(16.W, 8.BP)))
        ))

        val writeIdx = RegInit(0.U(8.W))
        val stride = 2.U
        val overflow = Reg(Bool())

        val overflowBits = RegInit(0.U(64.W))

        // saturation bounds
        val maxVal = FixedPoint.fromDouble(math.pow(2, 7) - math.pow(2, -8), 16.W, 8.BP)
        val minVal = FixedPoint.fromDouble(-math.pow(2, 7), 16.W, 8.BP)
        
        val reg_rd = Reg(UInt(5.W))
        val reg_xd = Reg(Bool())
        val reg_baseAddr = Reg(UInt(xLen.W))
        val reg_dprv = Reg(UInt(2.W))
        val reg_numElements = (((N * N) + 3) / 4).U
        val reg_inputTileType = Reg(UInt(10.W)) 

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

        // Loop counters
        val inRow = RegInit(0.U(log2Ceil(N+K-1).W))
        val inCol = RegInit(0.U(log2Ceil(N+K-1).W))

        val inRowStart = RegInit(0.U(log2Ceil(N+K-1).W))
        val inRowEnd = RegInit(0.U(log2Ceil(N+K-1).W))
        val inColStart = RegInit(0.U(log2Ceil(N+K-1).W))
        val inColEnd = RegInit(0.U(log2Ceil(N+K-1).W))

        val outIdx = RegInit(0.U((2 * log2Ceil(N)).W))

        // Output coordinates
        val outRow = outIdx / N.U
        val outCol = outIdx % N.U

        //val count = RegInit(0.U(32.W))
        val acc_buffer = RegInit(0.F(32.W, 8.BP))
        val acc = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.F(32.W, 8.BP))))))
        // val acc_buffer = RegInit(0.F(16.W, 8.BP))
        // val acc = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.F(16.W, 8.BP))))))
        val result = RegInit(VecInit(Seq.fill(N)(VecInit(Seq.fill(N)(0.F(16.W, 8.BP))))))

        val a = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.F(16.W, 8.BP))))))
        val b = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.F(16.W, 8.BP))))))

        // Flags for pipeline state
        val lastOutputPixel = (inRow === (N - 1).U) && (inCol === (N - 1).U)
        val finishedAll = RegInit(false.B)

        //count := count + 1.U

        switch(state) {
            is(sIdle) {          
                when(cmd.fire && doCompute) {
                    printf(p"[RoCC][sIdle] Cmd fired. funct = ${funct}, rd = ${cmd.bits.inst.rd}, baseAddr = 0x${Hexadecimal(cmd.bits.rs1)}, inputTileType = ${reg_inputTileType}\n")
                    // Latch cmd.bits 
                    reg_rd := cmd.bits.inst.rd 
                    reg_xd := cmd.bits.inst.xd 
                    reg_baseAddr := cmd.bits.rs1
                    reg_dprv := cmd.bits.status.dprv
                    reg_inputTileType := cmd.bits.rs2
                    writeIdx := 0.U

                    when (reg_inputTileType === full) {
                        inRowStart := 0.U
                        inRowEnd := (N-1).U
                        inColStart := 0.U
                        inColEnd := (N-1).U
                    }.elsewhen (reg_inputTileType === center) {
                        inRowStart := pad.U
                        inRowEnd := (N+pad-1).U
                        inColStart := pad.U
                        inColEnd := (N+pad-1).U
                    }.elsewhen (reg_inputTileType === topLeft) {
                        inRowStart := 0.U
                        inRowEnd := (N-1).U
                        inColStart := 0.U
                        inColEnd := (N-1).U
                    }.elsewhen (reg_inputTileType === top) {
                        inRowStart := 0.U
                        inRowEnd := (N-1).U
                        inColStart := pad.U
                        inColEnd := (N+pad-1).U
                    }.elsewhen (reg_inputTileType === topRight) {
                        inRowStart := 0.U
                        inRowEnd := (N-1).U
                        inColStart := (2*pad).U
                        inColEnd := (N+2*pad-1).U
                    }.elsewhen (reg_inputTileType === left) {
                        inRowStart := pad.U
                        inRowEnd := (N+pad-1).U
                        inColStart := 0.U
                        inColEnd := (N-1).U
                    }.elsewhen (reg_inputTileType === right) {
                        inRowStart := pad.U
                        inRowEnd := (N+pad-1).U
                        inColStart := (2*pad).U
                        inColEnd := (N+2*pad-1).U
                    }.elsewhen (reg_inputTileType === bottomLeft) {
                        inRowStart := (2*pad).U
                        inRowEnd := (N+2*pad-1).U
                        inColStart := 0.U
                        inColEnd := (N-1).U
                    }.elsewhen (reg_inputTileType === bottom) {
                        inRowStart := (2*pad).U
                        inRowEnd := (N+2*pad-1).U
                        inColStart := pad.U
                        inColEnd := (N+pad-1).U
                    }.elsewhen (reg_inputTileType === bottomRight) {
                        inRowStart := (2*pad).U
                        inRowEnd := (N+2*pad-1).U
                        inColStart := (2*pad).U
                        inColEnd := (N+2*pad-1).U
                    }
           
                    for (i <- 0 until K) {
                        for (j <- 0 until K) {
                            acc(i)(j) := 0.F(32.W, 8.BP)
                            // acc(i)(j) := 0.F(16.W, 8.BP)
                            a(i)(j) := 0.F(16.W, 8.BP)
                            b(i)(j) := 0.F(16.W, 8.BP)
                        }
                    }
                    outIdx := 0.U
                    finishedAll := false.B
                   
                    state := sSetup
                }
            }
            is (sSetup) {
                inRow := inRowStart
                inCol := inColStart

                state := sLoad
            }
            is(sLoad) {    
                printf(p"[RoCC][sLoad] Loading input patch at inRow = $inRow, inCol = $inCol, inputTileType = $reg_inputTileType\n")        
                for (i <- 0 until K) {
                    for (j <- 0 until K) {
                        val x = WireDefault(0.U)
                        val y = WireDefault(0.U)
                        val valid = WireDefault(false.B)
                        
                        when (K.U === 1.U){
                            x := inRow
                            y := inCol
                        }.otherwise {
                            x := (inRow +& j.U - pad.U)
                            y := (inCol +& i.U - pad.U)
                        }
                        
                        
                        when (reg_inputTileType === full) {
                            valid := x >= 0.U && x < N.U && y >= 0.U && y < N.U
                        }.elsewhen (reg_inputTileType === center) {
                            valid := true.B
                        }.elsewhen (reg_inputTileType === topLeft) {
                            valid := x >= 0.U && x < (N+pad).U && y >= 0.U && y < (N+pad).U
                        }.elsewhen (reg_inputTileType === top) {
                            valid := x >= 0.U && x < (N+pad).U && y >= 0.U && y <= (N+2*pad-1).U
                        }.elsewhen (reg_inputTileType === topRight) {
                            valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U
                        }.elsewhen (reg_inputTileType === left) {
                            valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y < (N+pad).U
                        }.elsewhen (reg_inputTileType === right) {
                            valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U
                        }.elsewhen (reg_inputTileType === bottomLeft) {
                            valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y < (N+pad).U
                        }.elsewhen (reg_inputTileType === bottom) {
                            valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U
                        }.elsewhen (reg_inputTileType === bottomRight) {
                            valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U 
                        }

                        when(valid) {
                            a(i)(j) := kernel(j.U)(i.U)
                            b(i)(j) := input(x)(y)
                        }.otherwise {
                            a(i)(j) := 0.F(16.W, 8.BP)
                            b(i)(j) := 0.F(16.W, 8.BP)
                        }
                        
                        //printf(p"Cycle: $count, kerCol = ${i.U}, kerRow = ${j.U}, inCol = $inCol, inRow = $inRow, outIdx: $outIdx, x = $x, y = $y, valid = $valid\n")
                    }
                }
                state := sAcc1
            }
            is(sAcc1) {
                printf(p"[RoCC][sAcc1] Accumulating: state=$state\n")
                for (i <- 0 until K) {
                    for (j <- 0 until K) {
                        acc(i)(j) := acc(i)(j) + (a(i)(j) * b(i)(j))
                    }
                }
                state := sAcc2

                //printf(p"Cycle: $count, state: $state, a = $a, b = $b\n")
            }
            is(sAcc2) {
            	printf(p"[RoCC][sAcc2] Accumulating: state=$state\n")
                var sum = 0.F(32.W, 8.BP)
                // var sum = 0.F(16.W, 8.BP)
                for (i <- 0 until K) {
                    for (j <- 0 until K) {
                        sum = sum + acc(i)(j)
                        acc(i)(j) := 0.F(32.W, 8.BP)
                        // acc(i)(j) := 0.F(16.W, 8.BP)
                        a(i)(j) := 0.F(16.W, 8.BP)
                        b(i)(j) := 0.F(16.W, 8.BP)
                    }
                }
                acc_buffer := sum

                // After full kernel scan, increment input pos
                when(inCol === inColEnd) {
                    inCol := inColStart
                    when(inRow === inRowEnd) {
                        // All pixels done: wait 1 cycle to commit final result
                        finishedAll := true.B
                    }.otherwise  {
                        inRow := inRow + 1.U
                    }
                }.otherwise {
                    inCol := inCol + 1.U
                }

                //printf(p"Cycle: $count, state: $state, acc = $acc\n")
                
                state := writeResult
            }
            is(writeResult) {
                val index = (outRow * N.U) + outCol // flatten 2D index 
                when(acc_buffer > maxVal) {
                    result(outRow)(outCol) := maxVal // clamp to max.
                    overflowBits := overflowBits.bitSet(index, true.B)
                } .elsewhen(acc_buffer < minVal) {
                    result(outRow)(outCol) := minVal // clamp to min.
                    overflowBits := overflowBits.bitSet(index, true.B)
                } .otherwise {
                    result(outRow)(outCol) := (acc_buffer.asSInt()(15, 0)).asFixedPoint(8.BP) // cast to 16-bit fixed point
                    // no need to modify overflowBits; default is 0
                }       
                printf(p"[RoCC][writeResult] outIdx = $outIdx, outRow = $outRow, outCol = $outCol\n")
                printf(p"  acc_buffer = ${acc_buffer.asUInt()}, clamped value = ${result(outRow)(outCol).asUInt()}, overflow = $overflow\n")  

                acc_buffer := 0.F(32.W, 8.BP)
                // acc_buffer := 0.F(16.W, 8.BP)
                outIdx := outIdx + 1.U
                when(finishedAll) {
                    state := sWriteReq
                }.otherwise {
                    state := sLoad
                }
                //printf(p"Cycle: $count, state: $state, inCol = $inCol, inRow = $inRow, outIdx: $outIdx, " + 
                //p"acc_buffer: $acc_buffer, lastOutputPixel = $lastOutputPixel, finishedAll = $finishedAll\n")
                
            }

            is(sWriteReq) {
                when(writeIdx < reg_numElements) {
                    printf(p"[RoCC][sWriteReq] writeIdx = $writeIdx, reg_numElements = $reg_numElements\n")
                    io.mem.req.valid := true.B 
                    io.mem.req.bits.addr := reg_baseAddr + (writeIdx << 3)
                    io.mem.req.bits.tag := writeIdx + 1.U // must be non-zero
                    io.mem.req.bits.cmd := M_XWR
                    io.mem.req.bits.size := 3.U // 64 bits
                    io.mem.req.bits.signed := false.B 
                    io.mem.req.bits.phys := false.B 
                    io.mem.req.bits.dprv := reg_dprv

                    val flatIdx = writeIdx << 2

                    // Extract 4 elements
                    val vals = Wire(Vec(4, FixedPoint(16.W, 8.BP)))

                    for (i <- 0 until 4) {
                        val idx = flatIdx + i.U 
                        val row = idx / N.U 
                        val col = idx % N.U 
                        val inBounds = idx < (N * N).U 
                        vals(i) := Mux(inBounds, result(row)(col), 0.F(16.W, 8.BP))
                    }

                    val data = Cat(
                        vals(3).asSInt().asUInt()(15,0),
                        vals(2).asSInt().asUInt()(15,0),
                        vals(1).asSInt().asUInt()(15,0),
                        vals(0).asSInt().asUInt()(15,0)
                    )

                    io.mem.req.bits.data := data

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

            is(sWaitResp) {
                when(io.mem.resp.valid) {
                    //val tag = io.mem.resp.bits.tag 
                    when(io.mem.resp.bits.tag === reg_numElements) {
                        state := sDone
                        printf("[RoCC] All write responses received\n")
                    }
                }
            }

            is(sDone) {
                when(reg_xd && io.resp.ready) {
                    io.resp.valid := true.B 
                    io.resp.bits.rd := reg_rd 
                    io.resp.bits.data := overflowBits
                    state := sIdle
                    printf(p"[RoCC] Written data: ${io.resp.bits.data} to rd: ${io.resp.bits.rd} success back\n")
                }.elsewhen(!reg_xd) {
                    state := sIdle
                }
            }
        }
}
