class MACUnit extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(32.W))
        val b = Input(UInt(32.W))
        val en = Input(Bool())
        val out = Output(UInt(32.W))
    })

    val product = RegInit(0.U(64.W))
    when(io.en) {
        product := io.a * io.b
    }
    io.out := product
}

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
    val acc = RegInit(0.U(64.W))
    val acc1 = RegInit(0.U(64.W))
    val result = RegInit(VecInit(Seq.fill(N)(VecInit(Seq.fill(N)(0.U(64.W))))))

    val mac = Module(new MACUnit)
    mac.io.a := 0.U
    mac.io.b := 0.U
    mac.io.en := false.B

    io.output := result
    io.done := false.B

    // Flags for pipeline state
    val macWindowComplete = (kerRow === (K - 1).U && kerCol === (K - 1).U)
    val resetAcc = RegNext(macWindowComplete, init = false.B)
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
                acc := 0.U
                outIdx := 0.U
                finishedAll := false.B
                state := sCompute
            }
        }

        is(sCompute) {
            val x = inRow + kerRow - pad.U
            val y = inCol + kerCol - pad.U
            val valid = x >= 0.U && x < N.U && y >= 0.U && y < N.U
            count := count + 1.U

            when(valid) {
                mac.io.a := io.kernel(kerRow)(kerCol)
                mac.io.b := io.input(x)(y)
                mac.io.en := true.B
                acc := acc + (mac.io.a * mac.io.b)
            }

            

            // Kernel scan control
            when(kerCol === (K - 1).U) {
                kerCol := 0.U
                when(kerRow === (K - 1).U) {
                    //kerRow := 0.U

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
                kerCol := kerCol + 1.U
            }

            // Commit result from previous kernel window
            when(resetAcc) {
                acc1 := acc
                acc := 0.U
                kerCol := 0.U
                kerRow := 0.U
            }

            when(commitResultNext) {
                result(outRow)(outCol) := acc1
                acc1 := 0.U
                outIdx := outIdx + 1.U
                when(finishedAll) {
                    state := sDone
                }
            }
            printf(p"Cycle: $count, kerCol = $kerCol, kerRow = $kerRow, inCol = $inCol, inRow = $inRow, outIdx: $outIdx, " + 
            p"mac.a: ${mac.io.a}, mac.b: ${mac.io.b}, acc: $acc, acc1: $acc1, valid = $valid, macWindowComplete = $macWindowComplete, " + 
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



test(new ConvolutionFSM(N = 3, K = 3)) { c =>
    val input = Seq(
        Seq(1, 2, 3),
        Seq(4, 5, 6),
        Seq(7, 8, 9)
    )

    val kernel = Seq(
        Seq(1, 2, 3),
        Seq(4, 5, 6),
        Seq(7, 8, 9)
    )

    // Poke inputs
    for (i <- 0 until 3) {
        for (j <- 0 until 3) {
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
