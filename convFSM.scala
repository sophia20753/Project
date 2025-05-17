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

    val sIdle :: sLoad :: sAcc1 :: sAcc2 :: writeResult :: sDone :: Nil = Enum(6)
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
                inRow := 0.U
                inCol := 0.U

                when (io.input_tile_type === full) {
                    inRowStart := 0.U
                    inRowEnd := (N-1).U
                    inColStart := 0.U
                    inColEnd := (N-1).U
                }.elsewhen (io.input_tile_type === center) {
                    inRowStart := (pad-1).U
                    inRowEnd := (N+pad-2).U
                    inColStart := (pad-1).U
                    inColEnd := (N+pad-2).U
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
                state := sLoad
            }
        }

        is(sLoad) {
            
            for (i <- 0 until K) {
                for (j <- 0 until K) {
                    val x = WireDefault(0.U)
                    val y = WireDefault(0.U)
                    val valid = WireDefault(false.B)
                    
                    
                    when (io.input_tile_type === full) {
                        x := (inRow - pad.U +& j.U)(log2Ceil(N) + 1 - 1, 0)
                        y := (inCol - pad.U +& i.U)(log2Ceil(N) + 1 - 1, 0)
                        valid := x >= 0.U && x < N.U && y >= 0.U && y < N.U
                    }.elsewhen (io.input_tile_type === center) {
                        x := (inRow +& j.U)(log2Ceil(N) + 1 - 1, 0)
                        y := (inCol +& i.U)(log2Ceil(N) + 1 - 1, 0)
                        valid := true.B
                    }

                    when(valid) {
                        a(i)(j) := io.kernel(j.U)(i.U)
                        b(i)(j) := io.input(x)(y)
                    }.otherwise {
                        a(i)(j) := 0.U
                        b(i)(j) := 0.U
                    }
                    
                    printf(p"Cycle: $count, kerCol = ${i.U}, kerRow = ${j.U}, inCol = $inCol, inRow = $inRow, outIdx: $outIdx, x = $x, y = $y, valid = $valid\n")
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

            printf(p"Cycle: $count, state: $state, a = $a, b = $b\n")
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
                printf("here\n")
                when(inRow === inRowEnd) {
                    // All pixels done: wait 1 cycle to commit final result
                    finishedAll := true.B
                }.otherwise {
                    inRow := inRow + 1.U
                }
            }.otherwise {
                inCol := inCol + 1.U
            }

            printf(p"Cycle: $count, state: $state, acc = $acc\n")
            
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
            printf(p"Cycle: $count, state: $state, inCol = $inCol, inRow = $inRow, outIdx: $outIdx, " + 
            p"acc_buffer: $acc_buffer, lastOutputPixel = $lastOutputPixel, finishedAll = $finishedAll\n")
            
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
        Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
        Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
        Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
        Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
        Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
        Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
        Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
        Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
        Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
        Seq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
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
    //    Seq(0,0,0,0,0),
    //    Seq(0,1,2,3,0),
    //    Seq(0,4,5,6,0),
    //    Seq(0,7,8,9,0),
    //    Seq(0,0,0,0,0)
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
    c.io.input_tile_type.poke(4.U)
    for (i <- 0 until 11) {
        for (j <- 0 until 11) {
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
