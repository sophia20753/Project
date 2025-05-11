class Convolution extends Module {
    val io = IO(new Bundle {
        val enable = Input(Bool())
        val done = Output(Bool())

        val kernel = Input(Vec(3, Vec(3, UInt(32.W))))
        val in = Input(Vec(3, Vec(3, UInt(32.W))))
        val out = Output(Vec(3, Vec(3, UInt(32.W))))

    })
    val pad = 1
    val N = 3
    val K = 3

    val output = RegInit(VecInit(Seq.fill(N)(VecInit(Seq.fill(N)(0.U(32.W))))))
    io.out := output

    val idle :: compute :: done :: Nil = Enum(3)
    val state = RegInit(idle)

    val i = RegInit(0.U(8.W))
    val j = RegInit(0.U(8.W))

    val m = RegInit(0.U(8.W))
    val n = RegInit(0.U(8.W))

    val sum = RegInit(0.U(64.W))

    val x = RegInit(0.U(8.W))
    val y = RegInit(0.U(8.W))

    io.done := (state === done)
    
    switch(state) {
        is(idle) {
            when(io.enable) {
                i := 0.U 
                j := 0.U 
                m := 0.U 
                n := 0.U 
                sum := 0.U
                state := compute
            }
        }
        is(compute) {
            x := i + m - pad.U 
            y := j + n - pad.U 
            val valid = x >= 0.U && x < N.U && y >= 0.U && y < N.U 

            when(valid) {
                sum := sum + (io.kernel(m)(n) * io.in(x)(y))
            }
            // increment n, m loops
            when(n === (K - 1).U) {
                n := 0.U
                when(m === (K - 1).U) {
                    m := 0.U
                    output(i)(j) := sum(31,0) // truncate
                    sum := 0.U

                    // increment j, i loops
                    when(j === (N - 1).U) {
                        j := 0.U
                        when(i === (N - 1).U) {
                            state := done
                        }.otherwise {
                            i := i + 1.U
                        }
                    }.otherwise {
                        j := j + 1.U
                    }

                }.otherwise {
                    m := m + 1.U
                }
            }.otherwise {
                n := n + 1.U
            }
        }
        is(done) {
            when (!io.enable) {
                state := idle
            }
        }
    }
}


test(new Convolution) { c =>

    val kernel = Seq(Seq(1.U, 2.U, 3.U), Seq(4.U, 5.U, 6.U), Seq(7.U, 8.U, 9.U))
    val input = Seq(Seq(1.U, 2.U, 3.U), Seq(4.U, 5.U, 6.U), Seq(7.U, 8.U, 9.U))
    val expected = Seq(
        Seq(94.U, 154.U, 106.U),
        Seq(186.U, 285.U, 186.U),
        Seq(106.U, 154.U, 94.U)
    )

    // Load inputs
    for (i <- 0 until 3) {
        for (j <- 0 until 3) {
            c.io.in(i)(j).poke(input(i)(j))
            c.io.kernel(i)(j).poke(kernel(i)(j))
        }
    }

    c.io.enable.poke(true.B)
    c.clock.step()
    c.io.enable.poke(false.B)

    while (!c.io.done.peek().litToBoolean) {
        c.clock.step()
    }

    // Check outputs
    for (i <- expected.indices) {
        for (j <- expected(i).indices) {
            val actual = c.io.out(i)(j).peek()
            val expectVal = expected(i)(j)
            assert(actual.litValue == expectVal.litValue,
              s"Mismatch at ($i, $j): got ${actual.litValue}, expected ${expectVal.litValue}")
        }
    }

    println("Convolution result matches expected output.")
}