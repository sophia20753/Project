class MemPortIO extends Bundle {
    val req = Decoupled(new Bundle {
        val addr = UInt(32.W)
        val funct = UInt(3.W)
        val typ = UInt(3.W)
    })
    val resp = Flipped(Valid(UInt(32.W)))
}

class KernelLoadCmd extends Bundle {
    val valid = Bool()
    val rs1 = UInt(32.W) // bits[1:0] encode size: 0=1x1, 1=3x3, 2=5x5
    val rs2 = UInt(32.W) // base address
}

class KernelLoader extends Module {
    val io = IO(new Bundle {
        val cmd = Input(new KernelLoadCmd)
        val done = Output(Bool())
        val mem = new MemPortIO
        val kernel = Output(Vec(5, Vec(5, UInt(32.W)))) // Max. 5x5 kernel 
    })

    // FSM states
    val sIdle :: sLoad :: sWaitMem :: sDone :: Nil = Enum(4)
    val state = RegInit(sIdle)

    // Internal registers
    val kernelSize = Reg(UInt(2.W))
    val baseAddr = Reg(UInt(32.W))

    val kerRow = RegInit(0.U(3.W))
    val kerCol = RegInit(0.U(3.W))
    val kernel = Reg(Vec(5, Vec(5, UInt(32.W))))

    // Decode size
    val kernelDim = Wire(UInt(3.W))
    kernelDim := MuxLookup(kernelSize, 1.U, Seq(
        0.U -> 1.U,
        1.U -> 3.U,
        2.U -> 5.U
    ))

    val index = kerRow * kernelDim + kerCol
    val byteOffset = index << 2 // index * 4 (for word addressing)
    val loadAddr = baseAddr + byteOffset

    val loadDone = (kerRow === (kernelDim - 1.U)) && (kerCol === (kernelDim - 1.U)) // possible timing condition

    io.mem.req.valid := false.B 
    io.mem.req.bits.addr := loadAddr 
    io.mem.req.bits.funct := "b000".U // M_XRD
    io.mem.req.bits.typ := "b010".U // MT_W
    io.done := false.B 
    io.kernel := kernel 

    switch(state) {
        is(sIdle) {
            when(io.cmd.valid) {
                kernelSize := io.cmd.rs1(1, 0)
                baseAddr := io.cmd.rs2 
                kerRow := 0.U 
                kerCol := 0.U 
                state := sLoad
            }
        }

        is(sLoad) {
            io.mem.req.valid := true.B 
            when(io.mem.req.ready) {
                state := sWaitMem 
            }
        }
        // TODO: DMA bulk memory transfer
        is(sWaitMem) {
            when(io.mem.resp.valid) {
                kernel(kerRow)(kerCol) := io.mem.resp.bits 

                when(kerCol === (kernelDim - 1.U)) {
                    kerCol := 0.U 
                    kerRow := kerRow + 1.U 
                }.otherwise {
                    kerCol := kerCol + 1.U 
                }
                state := Mux(loadDone, sDone, sLoad)
            }
        }

        is(sDone) {
            io.done := true.B 
            when(!io.cmd.valid) {
                state := sIdle
            }
        }
    }
}

test(new KernelLoader) { c =>
    val baseAddr = 0x1000L
    val kernelValues = Seq(
        1, 2, 3,
        4, 5, 6,
        7, 8, 9
    )

    // doLoadKernel
    c.io.cmd.valid.poke(true.B)
    c.io.cmd.rs1.poke(1.U) // kernel size = 3x3
    c.io.cmd.rs2.poke(baseAddr.U)

    c.clock.step(1)
    c.io.cmd.valid.poke(false.B)

    var loadIndex = 0

    for (i <- 0 until 9) {
        // Wait for memory request
        while (!c.io.mem.req.valid.peek().litToBoolean) {
            c.clock.step()
        }

        // Set ready to accept the request
        c.io.mem.req.ready.poke(true.B)
        c.clock.step()
        c.io.mem.req.ready.poke(false.B)

        // Simulate one-cycle delay, then send memory response
        c.io.mem.resp.valid.poke(true.B)
        c.io.mem.resp.bits.poke(kernelValues(i).U)
        c.clock.step()
        c.io.mem.resp.valid.poke(false.B)
    }
    // Final step to let FSM transition to done
    c.clock.step(2)

    println("Kernel loaded:")
    for (i <- 0 until 3) {
        for (j <- 0 until 3) {
            val value = c.io.kernel(i)(j).peek().litValue
            print(f"$value%4d")
        }
        println()
    }
}