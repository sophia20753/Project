class Convolution extends Module {
    val io = IO(new Bundle {
        val enable = Input(Bool())
        val done = Output(Bool())

        val kernel = Input(Vec(3, Vec(3, UInt(32.W))))
        val in = Input(Vec(3, Vec(3, UInt(32.W))))
        val out = Output(Vec(3, Vec(3, UInt(32.W))))

    })

    val idle :: compute :: done :: Nil = Enum(3)
    val state = RegInit(0.U(2.W))
    val curr = RegInit(0.U(32.W))
    val pad = (3-1)/2
    val result = RegInit(VecInit(Seq.fill(3)(VecInit(Seq.fill(3)(0.U(32.W))))))

    // Initialize outputs with default values
    io.done := false.B
    for (i <- 0 until 3) {
        for (j <- 0 until 3) {
            io.out(i)(j) := result(i)(j)
        }
    }

    when (state === idle) {
        when(io.enable) {
            state := compute
        }
    }.elsewhen (state === compute) {
        // Perform convolution operation
        for (or <- 0 until 3) {
            for (oc <- 0 until 3) {
                for (kr <- 0 until 3) {
                    for (kc <- 0 until 3) {
                        when ((or.U + kr.U - pad.U) >= 0.U & (or.U + kr.U - pad.U) < 3.U & (oc.U + kc.U - pad.U) >= 0.U & (oc.U + kc.U - pad.U) < 3.U) {
                            curr := curr + (io.kernel(kr)(kc) * io.in(or + kr - pad)(oc + kc - pad))
                        }
                    }
                }
                result(or)(oc) := curr
            }
        }
        state := done
    }.elsewhen (state === done) {
        io.done := true.B
        when(!io.enable) {
            state := idle
        }
    }

}


test(new Convolution) { c =>

    val kernel = Seq(Seq(1.U, 2.U, 3.U), Seq(4.U, 5.U, 6.U), Seq(7.U, 8.U, 9.U))
    val input = Seq(Seq(1.U, 2.U, 3.U), Seq(4.U, 5.U, 6.U), Seq(7.U, 8.U, 9.U))
    val expected = Seq(Seq(30.U, 36.U, 42.U), Seq(84.U, 90.U, 96.U), Seq(138.U, 144.U, 150.U))

    c.io.enable.poke(false.B)
    c.clock.step()

    for (i <- kernel.indices) {
        for (j <- kernel(i).indices) {
            c.io.kernel(i)(j).poke(kernel(i)(j))
            c.io.in(i)(j).poke(input(i)(j))
        }
    }

    c.io.enable.poke(true.B)
    c.clock.step()

    for (i <- expected.indices) {
        for (j <- expected(i).indices) {
            c.clock.step()
            c.io.out(i)(j).expect(expected(i)(j))
        }
    }
    c.io.done.expect(true.B)

}