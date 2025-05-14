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
    val acc = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.U(64.W))))))
    val result = RegInit(VecInit(Seq.fill(N)(VecInit(Seq.fill(N)(0.U(64.W))))))

    val a = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.U(32.W))))))
    val b = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.U(32.W))))))

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
                for (i <- 0 until K) {
                    for (j <- 0 until K) {
                        acc(i)(j) := 0.U
                        a(i)(j) := 0.U
                        b(i)(j) := 0.U
                    }
                }
                outIdx := 0.U
                finishedAll := false.B
                state := sCompute
            }
        }

        is(sCompute) {
            count := count + 1.U
            
            var x = 0.U
            var y = 0.U
            var valid = false.B

            for (i <- 0 until K) {
                for (j <- 0 until K) {
                    x = (inRow +& kerRow - pad.U + j.U)(log2Ceil(N) + 1 - 1, 0)
                    y = (inCol +& kerCol - pad.U + i.U)(log2Ceil(N) + 1 - 1, 0)
                    valid = x >= 0.U && x < N.U && y >= 0.U && y < N.U && kerRow < K.U && kerCol < K.U

                    when(valid) {
                        a(i)(j) := io.kernel(kerRow+j.U)(kerCol+i.U)
                        b(i)(j) := io.input(x)(y)
                    }.otherwise {
                        a(i)(j) := 0.U
                        b(i)(j) := 0.U
                    }
                    acc(i)(j) := acc(i)(j) + (a(i)(j) * b(i)(j))
                }
            }

            

            // Kernel scan control
            when(kerCol >= (K - 1).U) {
                kerCol := 0.U
                when(kerRow >= (K - 1).U) {
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
                    kerRow := kerRow + K.U
                }
            }.otherwise {
                kerCol := kerCol + K.U
            }
            
            
            // Commit result from previous kernel window
            when(resetAcc) {
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
            printf(p"Cycle: $count, kerCol = $kerCol, kerRow = $kerRow, inCol = $inCol, inRow = $inRow, outIdx: $outIdx, " + 
            p"acc_buffer: $acc_buffer, macWindowComplete = $macWindowComplete, " + 
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




test(new ConvolutionFSM(N = 9, K = 3)) { c =>
    c.clock.setTimeout(10000)

    val input = Seq(
        Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Seq(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)
    )

    //val input = Seq(
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8),
    //    Seq(1, 2, 3, 4, 5, 6, 7, 8)
    //)

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
    for (i <- 0 until 9) {
        for (j <- 0 until 9) {
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
    for (i <- 0 until 9) {
        for (j <- 0 until 9) {
            print(f"${c.io.output(i)(j).peek().litValue}%5d")
        }
        println()
    }
}
