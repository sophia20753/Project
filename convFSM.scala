class ConvolutionFSM(val N: Int, val K: Int) extends Module {
    val pad = (K - 1) / 2

    val io = IO(new Bundle {
        val enable = Input(Bool())
        val done = Output(Bool())

        val kernel = Input(Vec(K, Vec(K, UInt(32.W))))
        val input = Input(Vec(N, Vec(N, UInt(32.W))))
        val output = Output(Vec(N, Vec(N, UInt(64.W))))
    })

    val sIdle :: sCompute :: sDone :: Nil = Enum(3)
    val state = RegInit(sIdle)

    // Loop counters
    val inRow = RegInit(0.U(log2Ceil(N).W))
    val inCol = RegInit(0.U(log2Ceil(N).W))
    val kerRow = RegInit(0.U(log2Ceil(K).W))
    val kerCol = RegInit(0.U(log2Ceil(K).W))
    val outIdx = RegInit(0.U((2 * log2Ceil(N)).W))

    // Output coordinates
    val outRow = outIdx / N.U
    val outCol = outIdx % N.U

    val count = RegInit(0.U(32.W))
    val acc_buffer = RegInit(0.U(64.W))
    val acc1 = RegInit(0.U(64.W))
    val acc2 = RegInit(0.U(64.W))
    val acc3 = RegInit(0.U(64.W))
    val result = RegInit(VecInit(Seq.fill(N)(VecInit(Seq.fill(N)(0.U(64.W))))))

    val a1 = RegInit(0.U(32.W))
    val b1 = RegInit(0.U(32.W))
    val a2 = RegInit(0.U(32.W))
    val b2 = RegInit(0.U(32.W))
    val a3 = RegInit(0.U(32.W))
    val b3 = RegInit(0.U(32.W))

    io.output := result
    io.done := false.B

    // Flags for pipeline state
    val macWindowComplete = (kerRow >= (K - 1).U && kerCol >= (K - 1).U)
    val stall = RegNext(macWindowComplete, init = false.B)
    val resetAcc = RegNext(stall, init = false.B)
    val commitResultNext = RegNext(resetAcc, init = false.B)
    val lastOutputPixel = (inRow === (N - 1).U) && (inCol === (N - 1).U)
    val finishedAll = RegInit(false.B)

    switch(state) {
        is(sIdle) {
            when(io.enable) {
                inRow := 0.U
                inCol := 0.U
                kerRow := 0.U
                kerCol := 0.U
                acc1 := 0.U
                acc2 := 0.U
                acc3 := 0.U
                outIdx := 0.U
                finishedAll := false.B
                state := sCompute
            }
        }

        is(sCompute) {
            val x1 = (inRow +& kerRow - pad.U)(log2Ceil(N) + 1 - 1, 0)
            val y1 = (inCol +& kerCol - pad.U)(log2Ceil(N) + 1 - 1, 0)
            val valid1 = x1 >= 0.U && x1 < N.U && y1 >= 0.U && y1 < N.U && kerRow < K.U && kerCol < K.U

            val x2 = (inRow +& kerRow - pad.U)(log2Ceil(N) + 1 - 1, 0)
            val y2 = (inCol +& kerCol - pad.U + 1.U)(log2Ceil(N) + 1 - 1, 0)
            val valid2 = x2 >= 0.U && x2 < N.U && y2 >= 0.U && y2 < N.U && kerRow < K.U && kerCol < K.U

            val x3 = (inRow +& kerRow - pad.U)(log2Ceil(N) + 1 - 1, 0)
            val y3 = (inCol +& kerCol - pad.U + 2.U)(log2Ceil(N) + 1 - 1, 0)
            val valid3 = x3 >= 0.U && x3 < N.U && y3 >= 0.U && y3 < N.U && kerRow < K.U && kerCol < K.U
            count := count + 1.U
            

            when(valid1) {
                a1 := io.kernel(kerRow)(kerCol)
                b1 := io.input(x1)(y1)
            }.otherwise {
                a1 := 0.U
                b1 := 0.U
            }
            acc1 := acc1 + (a1 * b1)

            when(valid2) {
                a2 := io.kernel(kerRow)(kerCol+1.U)
                b2 := io.input(x2)(y2)
            }.otherwise {
                a2 := 0.U
                b2 := 0.U
            }
            acc2 := acc2 + (a2 * b2)

            when(valid3) {
                a3 := io.kernel(kerRow)(kerCol+2.U)
                b3 := io.input(x3)(y3)
            }.otherwise {
                a3 := 0.U
                b3 := 0.U
            }
            acc3 := acc3 + (a3 * b3)
            

            // Kernel scan control
            when(kerCol >= (K - 1).U) {
                kerCol := 0.U
                when(kerRow === (K - 1).U) {
                    kerRow := 0.U
                    // After full kernel scan, increment input pos
                    when(inCol === (N - 1).U) {
                        inCol := 0.U
                        when(inRow === (N - 1).U) {
                            // All pixels done: wait 1 cycle to commit final result
                            finishedAll := true.B
                        }.otherwise {
                            inRow := inRow + 1.U
                        }
                    }.otherwise {
                        inCol := inCol + 1.U
                    }
                }.otherwise {
                    kerRow := kerRow + 1.U
                }
            }.otherwise {
                kerCol := kerCol + 3.U
            }

            // Commit result from previous kernel window
            when(resetAcc) {
                acc_buffer := acc1 + acc2 + acc3

                acc1 := 0.U
                a1 := 0.U
                b1 := 0.U

                acc2 := 0.U
                a2 := 0.U
                b2 := 0.U

                acc3 := 0.U
                a3 := 0.U
                b3 := 0.U

                kerCol := 0.U
                kerRow := 0.U
            }

            when(commitResultNext) {
                result(outRow)(outCol) := acc_buffer
                acc_buffer := 0.U
                outIdx := outIdx + 1.U
                when(finishedAll) {
                    state := sDone
                }
            }
            printf(p"Cycle: $count, kerCol = $kerCol, kerRow = $kerRow, inCol = $inCol, inRow = $inRow, x1 = $x1, y1 = $y1, x2 = $x2, y2 = $y2, x3 = $x3, y3 = $y3, outIdx: $outIdx, " + 
            p"a1: ${a1}, b1: ${b1}, a2: ${a2}, b2: ${b2}, a3: ${a3}, b3: ${b3}, acc1: $acc1, acc2: $acc2, acc3: $acc3, acc_buffer: $acc_buffer, valid1 = $valid1, valid2 = $valid2, valid3 = $valid3, macWindowComplete = $macWindowComplete, " + 
            p"resetAcc = $resetAcc, commitResultNext = $commitResultNext, lastOutputPixel = $lastOutputPixel, finishedAll = $finishedAll\n")
            
        }

        is(sDone) {
            io.done := true.B
            when(!io.enable) {
                state := sIdle
            }
        }
    }
}




test(new ConvolutionFSM(N = 8, K = 3)) { c =>
    c.clock.setTimeout(10000)

    //val input = Seq(
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)
    //)

    val input = Seq(
        Seq(1, 2, 3, 4, 5, 6, 7, 8),
        Seq(1, 2, 3, 4, 5, 6, 7, 8),
        Seq(1, 2, 3, 4, 5, 6, 7, 8),
        Seq(1, 2, 3, 4, 5, 6, 7, 8),
        Seq(1, 2, 3, 4, 5, 6, 7, 8),
        Seq(1, 2, 3, 4, 5, 6, 7, 8),
        Seq(1, 2, 3, 4, 5, 6, 7, 8),
        Seq(1, 2, 3, 4, 5, 6, 7, 8)
    )

    //val input = Seq(
    //    Seq(1,2,3),
    //    Seq(4,5,6),
    //    Seq(7,8,9)
    //)

    val kernel = Seq(
        Seq(1,2,3),
        Seq(4,5,6),
        Seq(7,8,9)
    )

    //val kernel = Seq(
    //    Seq(1, 2, 3, 4, 5),
    //    Seq(6, 7, 8, 9, 10),
    //    Seq(11, 12, 13, 14, 15),
    //    Seq(16, 17, 18, 19, 20),
    //    Seq(21, 22, 23, 24, 25)
    //)

    // Poke inputs
    for (i <- 0 until 8) {
        for (j <- 0 until 8) {
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
