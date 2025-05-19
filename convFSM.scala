class ConvolutionFSM(val N: Int, val K: Int) extends Module {
    val pad = (K - 1) / 2

    val io = IO(new Bundle {
        val enable = Input(Bool())
        val done = Output(Bool())

        val kernel = Input(Vec(K, Vec(K, UInt(32.W))))
        val input = Input(Vec(N+K-1, Vec(N+K-1, UInt(32.W))))
        val input_tile_type = Input(UInt(10.W))
        val output = Output(Vec(N, Vec(N, UInt(64.W))))
    })

    val sIdle :: sSetup ::sLoad :: sAcc1 :: sAcc2 :: writeResult :: sDone :: Nil = Enum(7)
    val topLeft :: top :: topRight :: left :: center :: right :: bottomLeft :: bottom :: bottomRight :: full :: Nil = Enum(10)
    val state = RegInit(sIdle)

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

    val count = RegInit(0.U(32.W))
    val acc_buffer = RegInit(0.U(64.W))
    val acc = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.U(64.W))))))
    val result = RegInit(VecInit(Seq.fill(N)(VecInit(Seq.fill(N)(0.U(64.W))))))

    val a = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.U(32.W))))))
    val b = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.U(32.W))))))

    io.output := result
    io.done := false.B

    // Flags for pipeline state
    val lastOutputPixel = (inRow === (N - 1).U) && (inCol === (N - 1).U)
    val finishedAll = RegInit(false.B)

    count := count + 1.U

    switch(state) {
        is(sIdle) {
            
            when(io.enable) {

                when (io.input_tile_type === full) {
                    inRowStart := 0.U
                    inRowEnd := (N-1).U
                    inColStart := 0.U
                    inColEnd := (N-1).U
                }.elsewhen (io.input_tile_type === center) {
                    inRowStart := pad.U
                    inRowEnd := (N+pad-1).U
                    inColStart := pad.U
                    inColEnd := (N+pad-1).U
                }.elsewhen (io.input_tile_type === topLeft) {
                    inRowStart := 0.U
                    inRowEnd := (N-1).U
                    inColStart := 0.U
                    inColEnd := (N-1).U
                }.elsewhen (io.input_tile_type === top) {
                    inRowStart := 0.U
                    inRowEnd := (N-1).U
                    inColStart := pad.U
                    inColEnd := (N+pad-1).U
                }.elsewhen (io.input_tile_type === topRight) {
                    inRowStart := 0.U
                    inRowEnd := (N-1).U
                    inColStart := (2*pad).U
                    inColEnd := (N+2*pad-1).U
                }.elsewhen (io.input_tile_type === left) {
                    inRowStart := pad.U
                    inRowEnd := (N+pad-1).U
                    inColStart := 0.U
                    inColEnd := (N-1).U
                }.elsewhen (io.input_tile_type === right) {
                    inRowStart := pad.U
                    inRowEnd := (N+pad-1).U
                    inColStart := (2*pad).U
                    inColEnd := (N+2*pad-1).U
                }.elsewhen (io.input_tile_type === bottomLeft) {
                    inRowStart := (2*pad).U
                    inRowEnd := (N+2*pad-1).U
                    inColStart := 0.U
                    inColEnd := (N-1).U
                }.elsewhen (io.input_tile_type === bottom) {
                    inRowStart := (2*pad).U
                    inRowEnd := (N+2*pad-1).U
                    inColStart := pad.U
                    inColEnd := (N+pad-1).U
                }.elsewhen (io.input_tile_type === bottomRight) {
                    inRowStart := (2*pad).U
                    inRowEnd := (N+2*pad-1).U
                    inColStart := (2*pad).U
                    inColEnd := (N+2*pad-1).U
                }

                
                for (i <- 0 until K) {
                    for (j <- 0 until K) {
                        acc(i)(j) := 0.U
                        a(i)(j) := 0.U
                        b(i)(j) := 0.U
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
            
            for (i <- 0 until K) {
                for (j <- 0 until K) {
                    val x = WireDefault(0.U)
                    val y = WireDefault(0.U)
                    val valid = WireDefault(false.B)
                    
                    when (K.U === 1.U)
                    {
                        x := inRow
                        y := inCol
                    }.otherwise {
                        x := (inRow +& j.U - pad.U)
                        y := (inCol +& i.U - pad.U)
                    }
                    
                    
                    when (io.input_tile_type === full) {
                        valid := x >= 0.U && x < N.U && y >= 0.U && y < N.U
                    }.elsewhen (io.input_tile_type === center) {
                        valid := true.B
                    }.elsewhen (io.input_tile_type === topLeft) {
                        valid := x >= 0.U && x < (N+pad).U && y >= 0.U && y < (N+pad).U
                    }.elsewhen (io.input_tile_type === top) {
                        valid := x >= 0.U && x <= N.U && y >= 0.U && y <= N.U
                    }.elsewhen (io.input_tile_type === topRight) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U
                    }.elsewhen (io.input_tile_type === left) {
                        valid := x >= 0.U && x <= (N+pad).U && y >= 0.U && y < (N+pad).U
                    }.elsewhen (io.input_tile_type === right) {
                        valid := x >= 0.U && x <= (N+pad).U && y >= 0.U && y <= (N+2*pad-1).U
                    }.elsewhen (io.input_tile_type === bottomLeft) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y < (N+pad).U
                    }.elsewhen (io.input_tile_type === bottom) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+pad).U
                    }.elsewhen (io.input_tile_type === bottomRight) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U 
                    }

                    when(valid) {
                        a(i)(j) := io.kernel(j.U)(i.U)
                        b(i)(j) := io.input(x)(y)
                    }.otherwise {
                        a(i)(j) := 0.U
                        b(i)(j) := 0.U
                    }
                    
                    //printf(p"Cycle: $count, kerCol = ${i.U}, kerRow = ${j.U}, inCol = $inCol, inRow = $inRow, outIdx: $outIdx, x = $x, y = $y, valid = $valid\n")
                }
            }
            state := sAcc1
        }

        is(sAcc1) {
            

            for (i <- 0 until K) {
                for (j <- 0 until K) {
                    acc(i)(j) := acc(i)(j) + (a(i)(j) * b(i)(j))
                }
            }
            state := sAcc2

            //printf(p"Cycle: $count, state: $state, a = $a, b = $b\n")
        }

        is(sAcc2) {

            var sum = 0.U
            for (i <- 0 until K) {
                for (j <- 0 until K) {
                    sum = sum + acc(i)(j)
                    acc(i)(j) := 0.U
                    a(i)(j) := 0.U
                    b(i)(j) := 0.U
                }
            }
            acc_buffer := sum

            // After full kernel scan, increment input pos
            when(inCol === inColEnd) {
                inCol := inColStart
                when(inRow === inRowEnd) {
                    // All pixels done: wait 1 cycle to commit final result
                    finishedAll := true.B
                }.otherwise {
                    inRow := inRow + 1.U
                }
            }.otherwise {
                inCol := inCol + 1.U
            }

            //printf(p"Cycle: $count, state: $state, acc = $acc\n")
            
            state := writeResult
        }

        is(writeResult) {

            result(outRow)(outCol) := acc_buffer
            acc_buffer := 0.U
            outIdx := outIdx + 1.U
            when(finishedAll) {
                state := sDone
            }.otherwise {
                state := sLoad
            }
            //printf(p"Cycle: $count, state: $state, inCol = $inCol, inRow = $inRow, outIdx: $outIdx, " + 
            //p"acc_buffer: $acc_buffer, lastOutputPixel = $lastOutputPixel, finishedAll = $finishedAll\n")
            
        }

        is(sDone) {
            io.done := true.B
            when(!io.enable) {
                state := sIdle
            }
        }
    }
}




test(new ConvolutionFSM(N = 8, K = 1)) { c =>
    c.clock.setTimeout(10000)

    val input = Seq(
        Seq(1,2,3,4,5,6,7,8),
        Seq(9,10,11,12,13,14,15,16),
        Seq(17,18,19,20,21,22,23,24),
        Seq(25,26,27,28,29,30,31,32),
        Seq(33,34,35,36,37,38,39,40),
        Seq(41,42,43,44,45,46,47,48),
        Seq(49,50,51,52,53,54,55,56),
        Seq(57,58,59,60,61,62,63,64),
    )


    val kernel = Seq(
        Seq(1)
    )

    // Poke inputs
    c.io.input_tile_type.poke(4.U)
    for (i <- 0 until 8) {
        for (j <- 0 until 8) {
            c.io.input(i)(j).poke(input(i)(j).U)
        }
    }

    for (i <- 0 until 1) {
        for (j <- 0 until 1) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).U)
        }
    }

    c.io.enable.poke(true.B)
    c.clock.step()
    c.io.enable.poke(false.B)

    while (!c.io.done.peek().litToBoolean) {
        c.clock.step()
    }

    println("\nConvolution Output:")
    for (i <- 0 until 8) {
        for (j <- 0 until 8) {
            print(f"${c.io.output(i)(j).peek().litValue}%5d")
        }
        println()
    }
}

test(new ConvolutionFSM(N = 3, K = 3)) { c =>
    c.clock.setTimeout(10000)

    val input = Seq(
        Seq(1,2,3,0,0),
        Seq(4,5,6,0,0),
        Seq(7,8,9,0,0),
        Seq(0,0,0,0,0),
        Seq(0,0,0,0,0)
    )


    val kernel = Seq(
        Seq(1,2,3),
        Seq(4,5,6),
        Seq(7,8,9)
    )

    // Poke inputs
    c.io.input_tile_type.poke(0.U)
    for (i <- 0 until 5) {
        for (j <- 0 until 5) {
            c.io.input(i)(j).poke(input(i)(j).U)
        }
    }

    for (i <- 0 until 3) {
        for (j <- 0 until 3) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).U)
        }
    }

    c.io.enable.poke(true.B)
    c.clock.step()
    c.io.enable.poke(false.B)

    while (!c.io.done.peek().litToBoolean) {
        c.clock.step()
    }

    println("\nConvolution Output:")
    for (i <- 0 until 3) {
        for (j <- 0 until 3) {
            print(f"${c.io.output(i)(j).peek().litValue}%5d")
        }
        println()
    }
}


test(new ConvolutionFSM(N = 8, K = 3)) { c =>
    c.clock.setTimeout(10000)

    val input = Seq(
        Seq(1,2,3,4,5,6,7,8,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1),
        Seq(1,1,1,1,1,1,1,1,1,1),
        Seq(1,1,1,1,1,1,1,1,1,1)
    )


    val kernel = Seq(
        Seq(1,2,3),
        Seq(4,5,6),
        Seq(7,8,9)
    )

    // Poke inputs
    c.io.input_tile_type.poke(0.U)
    for (i <- 0 until 10) {
        for (j <- 0 until 10) {
            c.io.input(i)(j).poke(input(i)(j).U)
        }
    }

    for (i <- 0 until 3) {
        for (j <- 0 until 3) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).U)
        }
    }

    c.io.enable.poke(true.B)
    c.clock.step()
    c.io.enable.poke(false.B)

    while (!c.io.done.peek().litToBoolean) {
        c.clock.step()
    }

    println("\nConvolution Output:")
    for (i <- 0 until 8) {
        for (j <- 0 until 8) {
            print(f"${c.io.output(i)(j).peek().litValue}%5d")
        }
        println()
    }
}


test(new ConvolutionFSM(N = 5, K = 5)) { c =>
    c.clock.setTimeout(10000)

    val input = Seq(
        Seq(1,2,3,4,5,0,0,0,0),
        Seq(1,2,3,4,5,0,0,0,0),
        Seq(1,2,3,4,5,0,0,0,0),
        Seq(1,2,3,4,5,0,0,0,0),
        Seq(1,2,3,4,5,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0)
    )


    val kernel = Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20),
        Seq(21, 22, 23, 24, 25)
    )

    // Poke inputs
    c.io.input_tile_type.poke(0.U)
    for (i <- 0 until 9) {
        for (j <- 0 until 9) {
            c.io.input(i)(j).poke(input(i)(j).U)
        }
    }

    for (i <- 0 until 5) {
        for (j <- 0 until 5) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).U)
        }
    }

    c.io.enable.poke(true.B)
    c.clock.step()
    c.io.enable.poke(false.B)

    while (!c.io.done.peek().litToBoolean) {
        c.clock.step()
    }

    println("\nConvolution Output:")
    for (i <- 0 until 5) {
        for (j <- 0 until 5) {
            print(f"${c.io.output(i)(j).peek().litValue}%5d")
        }
        println()
    }
}

test(new ConvolutionFSM(N = 8, K = 5)) { c =>
    c.clock.setTimeout(10000)

    val input = Seq(
        Seq(1,2,3,4,5,6,7,8,1,1,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1,1,1),
        Seq(1,2,3,4,5,6,7,8,1,1,1,1),
        Seq(1,1,1,1,1,1,1,1,1,1,1,1),
        Seq(1,1,1,1,1,1,1,1,1,1,1,1),
        Seq(1,1,1,1,1,1,1,1,1,1,1,1),
        Seq(1,1,1,1,1,1,1,1,1,1,1,1)
    )


    val kernel = Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20),
        Seq(21, 22, 23, 24, 25)
    )

    // Poke inputs
    c.io.input_tile_type.poke(0.U)
    for (i <- 0 until 12) {
        for (j <- 0 until 12) {
            c.io.input(i)(j).poke(input(i)(j).U)
        }
    }

    for (i <- 0 until 5) {
        for (j <- 0 until 5) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).U)
        }
    }

    c.io.enable.poke(true.B)
    c.clock.step()
    c.io.enable.poke(false.B)

    while (!c.io.done.peek().litToBoolean) {
        c.clock.step()
    }

    println("\nConvolution Output:")
    for (i <- 0 until 8) {
        for (j <- 0 until 8) {
            print(f"${c.io.output(i)(j).peek().litValue}%5d")
        }
        println()
    }
}