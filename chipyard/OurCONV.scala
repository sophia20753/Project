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
		val sIdle :: sSetup :: sLoadFrame :: sAcc1 :: sAcc2 :: writeResult :: sWriteReq :: sWaitWriteResp :: sReadKernelReq :: sLoadKernelDone :: sReadInputReq :: sLoadInputDone :: sDone :: Nil = Enum(13)
		val state = RegInit(sIdle)
		val N = 8.U // Output size, 8 for 8x8 output

		val kernel = Reg(Vec(25, FixedPoint(16.W, 8.BP)))
		val kernelSize = Reg(UInt(2.W)) // Size of the kernel, 0: 1x1, 1: 3x3, 2: 5x5

		val reg_kernelBaseAddr = Reg(UInt(xLen.W))
		val reg_kernelDim = RegInit(5.U(3.W))
		val kernelNumElements = reg_kernelDim * reg_kernelDim

		val reg_inputBaseAddr = Reg(UInt(xLen.W)) // Base address for input
		val reg_inputDim = N + reg_kernelDim - 1.U
		val inputNumElements = reg_inputDim * reg_inputDim
		
		val input = Reg(Vec(144, FixedPoint(16.W, 8.BP)))

        val cmd = Queue(io.cmd)
        val funct = cmd.bits.inst.funct
        
        
        
		val doLoadKernel = (funct === 2.U)
        val doLoadInput = funct === 1.U
        val doCompute = funct === 3.U
        val memRespTag = io.mem.resp.bits.tag


		// *************************************
		// From doKernelLoad.scala
		val readReq = RegInit(0.U(8.W)) // tracks number of packed 64-bit words read requests sent
        val readResp = RegInit(0.U(8.W)) // tracks number of packed 64-bit words read requests responded 
        val totalKernelReadReq = ((kernelNumElements + 3.U) >> 2).asUInt

		val totalInputReadReq = ((inputNumElements + 3.U) >> 2).asUInt // total number of packed 64-bit words read requests for input
		// *************************************

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
		when (cmd.fire && state === sIdle && doLoadKernel) {
			reg_rd := cmd.bits.inst.rd 
			reg_xd := cmd.bits.inst.xd 
			reg_dprv := cmd.bits.status.dprv 
			reg_kernelBaseAddr := cmd.bits.rs1 
			when (cmd.bits.rs2 === 0.U) {
				reg_kernelDim := 1.U // 1x1 kernel
			}.elsewhen (cmd.bits.rs2 === 1.U) {
				reg_kernelDim := 3.U // 3x3 kernel
			}.otherwise {
				reg_kernelDim := 5.U // 5x5 kernel
			}
			kernelSize := cmd.bits.rs2(1,0)
			readReq := 0.U 
			readResp := 0.U

			state := sReadKernelReq
			printf("[RoCC] Read  kernel command received\n")
		}
		when (state === sReadKernelReq) {
			// Issue request
			val canIssue = readReq < totalKernelReadReq
			val canResp = readResp < totalKernelReadReq
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
						val v = data(15+16*i,0+16*i).asSInt.asFixedPoint(8.BP)
						kernel(flatIdx) := v
						printf("[RoCC] Wrote kernel(%d) = 0x%x\n",flatIdx, kernel(flatIdx).asUInt)
					}
				}
			}
			// Check for all requests issued, all requests responded
			when(!canIssue && !canResp) {
				state := sLoadKernelDone 
				printf("[RoCC] All kernel words loaded\n")
			}
		}
		when (state === sLoadKernelDone) {
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

		when (cmd.fire && state === sIdle && doLoadInput) {
			reg_rd := cmd.bits.inst.rd 
			reg_xd := cmd.bits.inst.xd 
			reg_dprv := cmd.bits.status.dprv 
			reg_inputBaseAddr := cmd.bits.rs1 
			reg_inputTileType := cmd.bits.rs2 // Tile type for input
			readReq := 0.U 
			readResp := 0.U 

			state := sReadInputReq
			printf("[RoCC] Read input command received\n")
		}
		when (state === sReadInputReq) {
			// Issue request
			val canIssue = readReq < totalInputReadReq
			val canResp = readResp < totalInputReadReq
			io.mem.req.valid := canIssue 
			io.mem.req.bits.addr := reg_inputBaseAddr + (readReq << 3)
			io.mem.req.bits.tag := readReq + 1.U // must be non-zero 
			io.mem.req.bits.cmd := M_XRD 
			io.mem.req.bits.size := log2Ceil(xLen/8).U 
			io.mem.req.bits.signed := false.B // change this
			io.mem.req.bits.data := 0.U // only for writes
			io.mem.req.bits.phys := false.B 
			io.mem.req.bits.dprv := reg_dprv 
			
			when(io.mem.req.fire) {
				readReq := readReq + 1.U 
				printf(p"[RoCC] Sent input read: addr=0x${Hexadecimal(io.mem.req.bits.addr)}, tag=${io.mem.req.bits.tag}\n")
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
					when(flatIdx < inputNumElements) {
						val v = data(15+16*i,0+16*i).asSInt.asFixedPoint(8.BP)
						input(flatIdx) := v
						printf("[RoCC] Wrote kernel(%d) = 0x%x\n",flatIdx, input(flatIdx).asUInt)
					}
				}
			}
			// Check for all requests issued, all requests responded
			when(!canIssue && !canResp) {
				state := sLoadInputDone 
				printf("[RoCC] All input words loaded\n")
			}
		}
		when (state === sLoadInputDone) {
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
			state := sLoadFrame 
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
							b(i)(j) := input(x * (N+2.U*pad) + y)
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

            acc_buffer := 0.F(32.W, 8.BP)
            outIdx := outIdx + 1.U
            when(finishedAll) {
                state := sWriteReq
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
                            state := sWaitWriteResp 
                            printf("[RoCC] All write requests issued, waiting for responses\n")
                        }
                    }
                }
		}

		when (state === sWaitWriteResp) {
			when(io.mem.resp.valid) { 
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
		
		

		

		// Memory req/resp handling during load states
        val doResp = cmd.bits.inst.xd
        val stallLoad = !io.mem.req.ready
        val stallResp = doResp && !io.resp.ready

        cmd.ready := !stallLoad && !stallResp && state === sIdle

        // PROC RESPONSE INTERFACE
        when(state =/= sDone && state =/= sLoadKernelDone && state =/= sLoadInputDone) {
            io.resp.valid := false.B 
            io.resp.bits := DontCare
        }

        io.busy := cmd.valid
        io.interrupt := false.B
        

        // Memory request interface
		
		when (state =/= sWriteReq && state =/= sReadKernelReq && state =/= sReadInputReq) {
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
		

	}
