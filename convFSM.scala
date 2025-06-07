class ConvolutionFSM(val N: Int, val K: Int) extends Module {
    import chisel3.experimental.FixedPoint

    val pad = (K - 1) / 2

    val io = IO(new Bundle {
        val enable = Input(Bool())
        val done = Output(Bool())

        val kernel = Input(Vec(K, Vec(K, FixedPoint(16.W, 8.BP))))
        val input = Input(Vec(N+K-1, Vec(N+K-1, FixedPoint(16.W, 8.BP))))
        val input_tile_type = Input(UInt(10.W))
        val output = Output(Vec(N, Vec(N, FixedPoint(32.W, 8.BP))))
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
    val acc_buffer = RegInit(0.F(32.W, 8.BP))
    val acc = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.F(32.W, 8.BP))))))
    val result = RegInit(VecInit(Seq.fill(N)(VecInit(Seq.fill(N)(0.F(32.W, 8.BP))))))

    val a = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.F(16.W, 8.BP))))))
    val b = RegInit(VecInit(Seq.fill(K)(VecInit(Seq.fill(K)(0.F(16.W, 8.BP))))))

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
                        acc(i)(j) := 0.F(32.W, 8.BP)
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
                        valid := x >= 0.U && x < (N+pad).U && y >= 0.U && y <= (N+2*pad-1).U
                    }.elsewhen (io.input_tile_type === topRight) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U
                    }.elsewhen (io.input_tile_type === left) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y < (N+pad).U
                    }.elsewhen (io.input_tile_type === right) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U
                    }.elsewhen (io.input_tile_type === bottomLeft) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y < (N+pad).U
                    }.elsewhen (io.input_tile_type === bottom) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U
                    }.elsewhen (io.input_tile_type === bottomRight) {
                        valid := x >= 0.U && x <= (N+2*pad-1).U && y >= 0.U && y <= (N+2*pad-1).U 
                    }

                    when(valid) {
                        a(i)(j) := io.kernel(j.U)(i.U)
                        b(i)(j) := io.input(x)(y)
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
            

            for (i <- 0 until K) {
                for (j <- 0 until K) {
                    acc(i)(j) := acc(i)(j) + (a(i)(j) * b(i)(j))
                }
            }
            state := sAcc2

            //printf(p"Cycle: $count, state: $state, a = $a, b = $b\n")
        }

        is(sAcc2) {

            var sum = 0.F(32.W, 8.BP)
            for (i <- 0 until K) {
                for (j <- 0 until K) {
                    sum = sum + acc(i)(j)
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

            //printf(p"Cycle: $count, state: $state, acc = $acc\n")
            
            state := writeResult
        }

        is(writeResult) {

            result(outRow)(outCol) := acc_buffer //made changes here !!! truncate
            acc_buffer := 0.F(32.W, 8.BP)
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

    import chisel3.experimental.FixedPoint

    val input = Seq(
        Seq(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0),
        Seq(9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0),
        Seq(17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0),
        Seq(25.0,26.0,27.0,28.0,29.0,30.0,31.0,32.0),
        Seq(33.0,34.0,35.0,36.0,37.0,38.0,39.0,40.0),
        Seq(41.0,42.0,43.0,44.0,45.0,46.0,47.0,48.0),
        Seq(49.0,50.0,51.0,52.0,53.0,54.0,55.0,56.0),
        Seq(57.0,58.0,59.0,60.0,61.0,62.0,63.0,64.0),
    )


    val kernel = Seq(
        Seq(-1.0)
    )

    // Poke inputs
    c.io.input_tile_type.poke(8.U)
    for (i <- 0 until 8) {
        for (j <- 0 until 8) {
            c.io.input(i)(j).poke(input(i)(j).F(16.W, 8.BP))
        }
    }

    for (i <- 0 until 1) {
        for (j <- 0 until 1) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).F(16.W, 8.BP))
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
            val fixedValue = c.io.output(i)(j).peek().litValue.toDouble / (1 << 8)
            print(f"$fixedValue%5.2f ")
        }
        println()
    }
}

test(new ConvolutionFSM(N = 3, K = 3)) { c =>
    c.clock.setTimeout(10000)

    import chisel3.experimental.FixedPoint

    val input = Seq(
        Seq(0,0,0,0,0),
        Seq(0,0,0,0,0),
        Seq(0,0,1,2,3),
        Seq(0,0,4,5,6),
        Seq(0,0,7,8,9)
    )


    val kernel = Seq(
        Seq(-1,-2,-3),
        Seq(-4,-5,-6),
        Seq(-7,-8,-9)
    )

    // Poke inputs
    c.io.input_tile_type.poke(8.U)
    for (i <- 0 until 5) {
        for (j <- 0 until 5) {
            c.io.input(i)(j).poke(input(i)(j).F(16.W, 8.BP))
        }
    }

    for (i <- 0 until 3) {
        for (j <- 0 until 3) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).F(16.W, 8.BP))
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
            val fixedValue = c.io.output(i)(j).peek().litValue.toDouble / (1 << 8)
            print(f"$fixedValue%5.2f ")
        }
        println()
    }
}


test(new ConvolutionFSM(N = 8, K = 3)) { c =>
    c.clock.setTimeout(10000)

    import chisel3.experimental.FixedPoint

    val input = Seq(
        Seq(0,0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0,0),
        Seq(0,0,-1,-2,-3,-4,-5,-6,-7,-8),
        Seq(0,0,-1,-2,-3,-4,-5,-6,-7,-8),
        Seq(0,0,-1,-2,-3,-4,-5,-6,-7,-8),
        Seq(0,0,-1,-2,-3,-4,-5,-6,-7,-8),
        Seq(0,0,-1,-2,-3,-4,-5,-6,-7,-8),
        Seq(0,0,-1,-2,-3,-4,-5,-6,-7,-8),
        Seq(0,0,-1,-2,-3,-4,-5,-6,-7,-8),
        Seq(0,0,-1,-2,-3,-4,-5,-6,-7,-8)
    )


    val kernel = Seq(
        Seq(1,2,3),
        Seq(4,5,6),
        Seq(7,8,9)
    )

    // Poke inputs
    c.io.input_tile_type.poke(8.U)
    for (i <- 0 until 10) {
        for (j <- 0 until 10) {
            c.io.input(i)(j).poke(input(i)(j).F(16.W, 8.BP))
        }
    }

    for (i <- 0 until 3) {
        for (j <- 0 until 3) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).F(16.W, 8.BP))
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
            val fixedValue = c.io.output(i)(j).peek().litValue.toDouble / (1 << 8)
            print(f"$fixedValue%5.2f ")
        }
        println()
    }
}


test(new ConvolutionFSM(N = 5, K = 5)) { c =>
    c.clock.setTimeout(10000)

    import chisel3.experimental.FixedPoint

    val input = Seq(
        Seq(0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,1,2,3,4,5),
        Seq(0,0,0,0,1,2,3,4,5),
        Seq(0,0,0,0,1,2,3,4,5),
        Seq(0,0,0,0,1,2,3,4,5),
        Seq(0,0,0,0,1,2,3,4,5)
    )


    val kernel = Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20),
        Seq(21, 22, 23, 24, 25)
    )

    // Poke inputs
    c.io.input_tile_type.poke(8.U)
    for (i <- 0 until 9) {
        for (j <- 0 until 9) {
            c.io.input(i)(j).poke(input(i)(j).F(16.W, 8.BP))
        }
    }

    for (i <- 0 until 5) {
        for (j <- 0 until 5) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).F(16.W, 8.BP))
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
            val fixedValue = c.io.output(i)(j).peek().litValue.toDouble / (1 << 8)
            print(f"$fixedValue%5.2f ")
        }
        println()
    }
}

test(new ConvolutionFSM(N = 8, K = 5)) { c =>
    c.clock.setTimeout(10000)

    import chisel3.experimental.FixedPoint

    val input = Seq(
        Seq(0,0,0,0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,0,0,0,0,0,0,0,0),
        Seq(0,0,0,0,1,2,3,4,5,6,7,8),
        Seq(0,0,0,0,1,2,3,4,5,6,7,8),
        Seq(0,0,0,0,1,2,3,4,5,6,7,8),
        Seq(0,0,0,0,1,2,3,4,5,6,7,8),
        Seq(0,0,0,0,1,2,3,4,5,6,7,8),
        Seq(0,0,0,0,1,2,3,4,5,6,7,8),
        Seq(0,0,0,0,1,2,3,4,5,6,7,8),
        Seq(0,0,0,0,1,2,3,4,5,6,7,8)
    )


    val kernel = Seq(
        Seq(1, 2, 3, 4, 5),
        Seq(6, 7, 8, 9, 10),
        Seq(11, 12, 13, 14, 15),
        Seq(16, 17, 18, 19, 20),
        Seq(21, 22, 23, 24, 25)
    )

    // Poke inputs
    c.io.input_tile_type.poke(8.U)
    for (i <- 0 until 12) {
        for (j <- 0 until 12) {
            c.io.input(i)(j).poke(input(i)(j).F(16.W, 8.BP))
        }
    }

    for (i <- 0 until 5) {
        for (j <- 0 until 5) {
            c.io.kernel(i)(j).poke((kernel(i)(j)).F(16.W, 8.BP))
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
            val fixedValue = c.io.output(i)(j).peek().litValue.toDouble / (1 << 8)
            print(f"$fixedValue%5.2f ")
        }
        println()
    }
}