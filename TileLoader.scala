class MemReq extends Bundle {
    val addr = UInt(32.W)
    val cmd = UInt(2.W) 
    val size = UInt(3.W) 
    val data = UInt(32.W) // Only used for writes
}

class MemResp extends Bundle {
    val data = UInt(32.W)
    val valid = Bool()
}

class ROCCMemoryInterface extends Bundle {
    val req = Decoupled(new MemReq)
    val resp = Flipped(Valid(new MemResp))
}

class TileLoader(val N: Int, val K: Int) extends Module {
    val tileSize = N + K - 1

    val io = IO(new Bundle {
        val start = Input(Bool()) // Start signal
        val baseAddr = Input(UInt(32.W)) // First element's memory address
        val mem = new ROCCMemoryInterface // Simulates memory interface for testing
        val done = Output(Bool()) // Tile loading complete
        val tile = Output(Vec(tileSize, Vec(tileSize, UInt(32.W)))) // Loaded tile
    })

    val tile = Reg(Vec(tileSize, Vec(tileSize, UInt(32.W))))
    val row = RegInit(0.U(log2Ceil(tileSize).W))
    val col = RegInit(0.U(log2Ceil(tileSize).W))
    val busy = RegInit(false.B)
    val done = RegInit(false.B)
    val waitingForResp = RegInit(false.B) // flag to track if memory request issued and still waiting for response

    io.tile := tile
    io.done := done 

    // Calculate the address for the current element
    val addrOffset = (row * tileSize.U + col) * 4.U // 4 bytes per element?
    val currentAddr = io.baseAddr + addrOffset

    // Default memory request
    io.mem.req.valid := false.B 
    io.mem.req.bits.addr := currentAddr 
    io.mem.req.bits.cmd := 0.U // 0: M_XRD (read)
    io.mem.req.bits.size := 2.U // 4 bytes
    io.mem.req.bits.data := 0.U 

    io.mem.resp.ready := true.B 

    // Start logic
    when(io.start && !busy) {
        busy := true.B 
        row := 0.U 
        col := 0.U 
        done := false.B 
        waitingForResp := false.B 
    }

    when(busy && !done) {       
        when(!waitingForResp) {
            io.mem.req.valid := true.B 
            when(io.mem.req.fire) {
                waitingForResp := true.B // requested memory
            }
        }.elsewhen(io.mem.resp.fire) {
            // Got a response, write element
            tile(row)(col) := io.mem.resp.bits.data
            waitingForResp := false.B // got data, new request next cycle
            // Advance to next element
            when(col === (tileSize - 1).U) {
                col := 0.U 
                when(row === (tileSize - 1).U) {
                    busy := false.B 
                    done := true.B 
                }.otherwise {
                    row := row + 1.U 
                }
            }.otherwise {
                col := col + 1.U
            }
        }
    }
}

test(new TileLoader(3, 3, 3)) { c =>
    // Hardcoded parameters
    val tileSize = 3 + 3 - 3 // = 3
    val baseAddr = 0x1000

    // Start the tile loading
    c.io.baseAddr.poke(baseAddr.U)
    c.io.start.poke(true.B)
    c.clock.step(1)
    c.io.start.poke(false.B)

    // Simulate memory responses
    while (!c.io.done.peek().litToBoolean) {
        if (c.io.mem.req.valid.peek().litToBoolean) {
            val addr = c.io.mem.req.bits.addr.peek().litValue.toInt
            val index = (addr - baseAddr) / 4
            val data = (index + 1) * 10

            c.io.mem.resp.valid.poke(true.B)
            c.io.mem.resp.bits.data.poke(data.U)
        } else {
            c.io.mem.resp.valid.poke(false.B)
        }

        c.clock.step(1)
    }

    // Print tile contents for verification
    println(s"\nLoaded tile $tileSize x $tileSize:")
    for (i <- 0 until tileSize) {
        for (j <- 0 until tileSize) {
            val index = i * tileSize + j
            val expected = (index + 1) * 10
            val actual = c.io.tile(i)(j).peek().litValue.toInt
            print(f"$actual%4d")
            assert(actual == expected)
        }
        println()
    }

    println("Tile loading test passed.")
}