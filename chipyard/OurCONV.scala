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
		val sIdle :: sLoadKernel :: sLoadInput :: sSetup :: sLoadFrame :: sAcc1 :: sAcc2 :: writeResult :: sWriteReq :: sWaitResp :: sDone :: Nil = Enum(11)
		val state = RegInit(sIdle)
		val N = 8.U // Output size, 8 for 8x8 output

		val kernel = Reg(Vec(25, FixedPoint(16.W, 8.BP)))
		val kernelSize = Reg(UInt(2.W)) // Size of the kernel, 0: 1x1, 1: 3x3, 2: 5x5
		
		val input = Reg(Vec(100, FixedPoint(16.W, 8.BP)))
        
		val busy = RegInit(VecInit(Seq.fill(125){false.B}))

        val cmd = Queue(io.cmd)
        val funct = cmd.bits.inst.funct
        
        val doPrint = funct === 0.U
        val loadIsInput = funct === 1.U
		val loadIsKernel = funct === 2.U
        val doLoad = funct === 1.U || funct === 2.U
        val doCompute = funct === 3.U
		val doSetKernelSize = funct === 4.U
        val memRespTag = io.mem.resp.bits.tag

		// *************************************
		// From convDoWrite.scala
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
        val reg_numElements = (((N * N) + 3.U) / 4.U)
        val reg_inputTileType = Reg(UInt(10.W)) 
		// *************************************

        //datapath
        val rs1 = cmd.bits.rs1
		val rs2 = cmd.bits.rs2
        
		// ************************************
		// Computation start

		val topLeft :: top :: topRight :: left :: center :: right :: bottomLeft :: bottom :: bottomRight :: full :: Nil = Enum(10)
		val tileType = RegInit(full) // Tile type for the convolution operation
		//val done = RegInit(false.B) // Flag to indicate completion of the operation

		// Loop counters
		val pad = kernelSize 
		val K = 2.U*pad+1.U // Kernel size, 1 for 1x1, 3 for 3x3, 5 for 5x5
		
		

		val inRow = RegInit(0.U(4.W))
		val inCol = RegInit(0.U(4.W))

		val inRowStart = RegInit(0.U(4.W))
		val inRowEnd = RegInit(0.U(4.W))
		val inColStart = RegInit(0.U(4.W))
		val inColEnd = RegInit(0.U(4.W))

		val outIdx = RegInit(0.U(6.W))
		// Output coordinates
		val outRow = outIdx / N
		val outCol = outIdx % N

		val count = RegInit(0.U(32.W))
		val acc_buffer = RegInit(0.F(32.W, 8.BP))
		val acc = RegInit(VecInit(Seq.fill(5)(VecInit(Seq.fill(5)(0.F(32.W, 8.BP))))))
		val result = RegInit(VecInit(Seq.fill(8)(VecInit(Seq.fill(8)(0.F(32.W, 8.BP))))))

		val a = RegInit(VecInit(Seq.fill(5)(VecInit(Seq.fill(5)(0.F(16.W, 8.BP))))))
		val b = RegInit(VecInit(Seq.fill(5)(VecInit(Seq.fill(5)(0.F(16.W, 8.BP))))))

		// Flags for pipeline state
		val lastOutputPixel = (inRow === (N - 1.U)) && (inCol === (N - 1.U))
		val finishedAll = RegInit(false.B)

		count := count + 1.U

		// State machine
		when (cmd.fire && state === sIdle && doCompute) {
			reg_rd := cmd.bits.inst.rd 
			reg_xd := cmd.bits.inst.xd 
			reg_baseAddr := cmd.bits.rs1
			reg_dprv := cmd.bits.status.dprv
			writeIdx := 0.U

			tileType := rs2 // Set tile type based on rs2
			when (rs2 === full) {
                    inRowStart := 0.U
                    inRowEnd := 7.U // for 8x8 output
                    inColStart := 0.U
                    inColEnd := 7.U // for 8x8 output
                }.elsewhen (rs2 === center) {
                    inRowStart := pad
                    inRowEnd := (N+pad-1.U)
                    inColStart := pad
                    inColEnd := (N+pad-1.U)
                }.elsewhen (rs2 === topLeft) {
                    inRowStart := 0.U
                    inRowEnd := 7.U // for 8x8 output
                    inColStart := 0.U
                    inColEnd := 7.U // for 8x8 output
                }.elsewhen (rs2 === top) {
                    inRowStart := 0.U
                    inRowEnd := 7.U // for 8x8 output
                    inColStart := pad
                    inColEnd := (N+pad-1.U)
                }.elsewhen (rs2 === topRight) {
                    inRowStart := 0.U
                    inRowEnd := 7.U // for 8x8 output
                    inColStart := (2.U*pad)
                    inColEnd := (N+2.U*pad-1.U)
                }.elsewhen (rs2 === left) {
                    inRowStart := pad
                    inRowEnd := (N+pad-1.U)
                    inColStart := 0.U
                    inColEnd := 7.U // for 8x8 output
                }.elsewhen (rs2 === right) {
                    inRowStart := pad
                    inRowEnd := (N+pad-1.U)
                    inColStart := (2.U*pad)
                    inColEnd := (N+2.U*pad-1.U)
                }.elsewhen (rs2 === bottomLeft) {
                    inRowStart := (2.U*pad)
                    inRowEnd := (N+2.U*pad-1.U)
                    inColStart := 0.U
                    inColEnd := 7.U // for 8x8 output
                }.elsewhen (rs2 === bottom) {
                    inRowStart := (2.U*pad)
                    inRowEnd := (N+2.U*pad-1.U)
                    inColStart := pad
                    inColEnd := (N+pad-1.U)
                }.elsewhen (rs2 === bottomRight) {
                    inRowStart := (2.U*pad)
                    inRowEnd := (N+2.U*pad-1.U)
                    inColStart := (2.U*pad)
                    inColEnd := (N+2.U*pad-1.U)
                }
			for (i <- 0 until 5) {
				for (j <- 0 until 5) {
					acc(i)(j) := 0.F(32.W, 8.BP)
					a(i)(j) := 0.F(16.W, 8.BP)
					b(i)(j) := 0.F(16.W, 8.BP)
				}
			}
			outIdx := 0.U
			finishedAll := false.B
			state := sSetup
			printf(p"Starting convolution with tile type: ${rs2}\n")	
		}

		when (state === sSetup) {
			inRow := inRowStart
			inCol := inColStart
			//done := false.B
			state := sLoadFrame // Load the first frame
		}

		when (state === sLoadFrame) {
			for (i <- 0 until 5) {
                for (j <- 0 until 5) {
                    val x = WireDefault(0.U)
                    val y = WireDefault(0.U)
                    val valid = WireDefault(false.B)
                    
					when (i.U < K && j.U < K) {
						when (pad === 0.U)
						{
							x := inRow
							y := inCol
						}.otherwise {
							x := (inRow +& j.U - pad)
							y := (inCol +& i.U - pad)
						}
						
						
						when (tileType === full) {
							valid := x >= 0.U && x < N && y >= 0.U && y < N
						}.elsewhen (tileType === center) {
							valid := true.B
						}.elsewhen (tileType === topLeft) {
							valid := x >= 0.U && x < (N+pad) && y >= 0.U && y < (N+pad)
						}.elsewhen (tileType === top) {
							valid := x >= 0.U && x < (N+pad) && y >= 0.U && y <= (N+2.U*pad-1.U)
						}.elsewhen (tileType === topRight) {
							valid := x >= 0.U && x <= (N+2.U*pad-1.U) && y >= 0.U && y <= (N+2.U*pad-1.U)
						}.elsewhen (tileType === left) {
							valid := x >= 0.U && x <= (N+2.U*pad-1.U) && y >= 0.U && y < (N+pad)
						}.elsewhen (tileType === right) {
							valid := x >= 0.U && x <= (N+2.U*pad-1.U) && y >= 0.U && y <= (N+2.U*pad-1.U)
						}.elsewhen (tileType === bottomLeft) {
							valid := x >= 0.U && x <= (N+2.U*pad-1.U) && y >= 0.U && y < (N+pad)
						}.elsewhen (tileType === bottom) {
							valid := x >= 0.U && x <= (N+2.U*pad-1.U) && y >= 0.U && y <= (N+2.U*pad-1.U)
						}.elsewhen (tileType === bottomRight) {
							valid := x >= 0.U && x <= (N+2.U*pad-1.U) && y >= 0.U && y <= (N+2.U*pad-1.U) 
						}

						when(valid) {
							a(i)(j) := kernel(j.U * K + i.U)
							b(i)(j) := input(x * K + y)
						}.otherwise {
							a(i)(j) := 0.F(16.W, 8.BP)
							b(i)(j) := 0.F(16.W, 8.BP)
						}
						
						printf(p"kerCol = ${i.U}, kerRow = ${j.U}, inCol = $inCol, inRow = $inRow, outIdx: $outIdx, x = $x, y = $y, valid = $valid\n")
					}
				}
            }
            state := sAcc1
		}

		when (state === sAcc1) {
			for (i <- 0 until 5) {
                for (j <- 0 until 5) {
					when (i.U < K && j.U < K) {
                    	acc(i)(j) := acc(i)(j) + (a(i)(j) * b(i)(j))
					}
                }
            }
            state := sAcc2

            printf(p"Cycle: ${count}, state: ${state}, a = ${a.asUInt}, b = ${b.asUInt}\n")
		}

		when (state === sAcc2) {
			val sum = (for (i <- 0 until 5; j <- 0 until 5) yield {
				Mux(i.U < K && j.U < K, acc(i)(j), 0.F(32.W, 8.BP))
				}).reduce(_ + _)
				
			acc_buffer := sum

			for (i <- 0 until 5; j <- 0 until 5) {
				when (i.U < K && j.U < K) {
					acc(i)(j) := 0.F(32.W, 8.BP)
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

            printf(p"Cycle: ${count}, state: ${state}, acc = ${acc.asUInt}\n")
            
            state := writeResult
		}

		when (state === writeResult) {
			
			// *********************************************
			// from convDoWrite.scala
			val index = (outRow * N) + outCol // flatten 2D index 
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

			// *********************************************

			//result(outRow)(outCol) := acc_buffer //make changes here !!! truncate
            acc_buffer := 0.F(32.W, 8.BP)
            outIdx := outIdx + 1.U
            when(finishedAll) {
                state := sWriteReq
				//done := true.B // Mark the operation as done
            }.otherwise {
                state := sLoadFrame // Load the next frame
            }
            printf(p"Cycle: ${count}, state: ${state}, inCol = ${inCol}, inRow = ${inRow}, outIdx: ${outIdx}, " + 
            p"acc_buffer: ${acc_buffer.asUInt}, lastOutputPixel = ${lastOutputPixel}, finishedAll = ${finishedAll}\n")
		}

		// ********************************************
		// From convDoWrite.scala
		when (state === sWriteReq) {
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
                        val row = idx / N 
                        val col = idx % N 
                        val inBounds = idx < (N * N) 
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

		when (state === sWaitResp) {
			when(io.mem.resp.valid) {
				//val tag = io.mem.resp.bits.tag 
				when(io.mem.resp.bits.tag === reg_numElements) {
					state := sDone
					printf("[RoCC] All write responses received\n")
				}
			}
		}

		when (state === sDone) {
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

		// ********************************************


		// Computation end
		//********************************************
		

		when (cmd.fire && doSetKernelSize) {
			kernelSize := cmd.bits.rs1(1, 0) // Set kernel size based on rs1 bits
		}

		when (doLoad && loadIsInput && cmd.fire) {
			state := sLoadInput // Input load
		}
		when (doLoad && loadIsKernel && cmd.fire) {
			state := sLoadKernel // Kernel load
		}

		when (doPrint && cmd.fire) {
			printf(p"Kernel size set to: ${kernelSize}\n")

			printf(p"kernel0  = ${kernel(0).asUInt}\n")
			printf(p"kernel1  = ${kernel(1).asUInt}\n")
			printf(p"kernel2  = ${kernel(2).asUInt}\n")
			printf(p"kernel3  = ${kernel(3).asUInt}\n")
			printf(p"kernel4  = ${kernel(4).asUInt}\n")
			printf(p"kernel5  = ${kernel(5).asUInt}\n")
			printf(p"kernel6  = ${kernel(6).asUInt}\n")
			printf(p"kernel7  = ${kernel(7).asUInt}\n")
			printf(p"kernel8  = ${kernel(8).asUInt}\n")
			printf(p"kernel9  = ${kernel(9).asUInt}\n")
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

			printf(p"input0  = ${input(0).asUInt}\n")
			printf(p"input1  = ${input(1).asUInt}\n")
			printf(p"input2  = ${input(2).asUInt}\n")
			printf(p"input3  = ${input(3).asUInt}\n")
			printf(p"input4  = ${input(4).asUInt}\n")
			printf(p"input5  = ${input(5).asUInt}\n")
			printf(p"input6  = ${input(6).asUInt}\n")
			printf(p"input7  = ${input(7).asUInt}\n")
			printf(p"input8  = ${input(8).asUInt}\n")
			printf(p"input9  = ${input(9).asUInt}\n")
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

		// Memory req/resp handling during load states
        when (io.mem.resp.valid && state === sLoadKernel) {
			printf(p"MEM RESP: tag=${memRespTag} data=${io.mem.resp.bits.data}\n")
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
        
        when (io.mem.req.fire) {
			when (state === sLoadKernel) {
            	busy(rs2) := true.B
			}
			when (state === sLoadInput) {
				busy(rs2+25.U) := true.B
			}
        }

        val doResp = cmd.bits.inst.xd
        val stallReg = busy(rs2)
        val stallLoad = doLoad && !io.mem.req.ready
        val stallResp = doResp && !io.resp.ready

        cmd.ready := !stallReg && !stallLoad && !stallResp && state === sIdle

        // PROC RESPONSE INTERFACE
        when(state =/= sDone) {
            io.resp.valid := false.B 
            io.resp.bits := DontCare
        }
        //io.resp.bits.rd := cmd.bits.inst.rd
        //io.resp.bits.data := 1.U

        io.busy := cmd.valid || busy.reduce(_ || _) || state =/= sIdle
        io.interrupt := false.B
        

        // Memory request interface
        io.mem.req.valid := cmd.valid && doLoad && !stallReg && !stallResp
        io.mem.req.bits.addr := rs1
        io.mem.req.bits.tag := rs2
        io.mem.req.bits.cmd := M_XRD
        io.mem.req.bits.size := log2Ceil(8).U
        io.mem.req.bits.signed := false.B
        io.mem.req.bits.data := 0.U
        io.mem.req.bits.phys := false.B
        io.mem.req.bits.dprv := cmd.bits.status.dprv

	}
