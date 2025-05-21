class ROCCInstruction extends Bundle {
    val opcode = UInt(7.W)
    val rd     = UInt(5.W)
    val funct3 = UInt(3.W)
    val rs1    = UInt(5.W)
    val rs2    = UInt(5.W)
    val funct7 = UInt(7.W)
}

class MatrixAddrLoader extends Module {
    val io = IO(new Bundle {
        val cmd = Flipped(Decoupled(new ROCCInstruction))

        // Expose internal signals for testing
        val test_startLoad       = Output(Bool())
        val test_isOutput        = Output(Bool())
        val test_loadInputAddr   = Output(UInt(32.W))
        val test_loadOutputAddr  = Output(UInt(32.W))
        val test_matrixLoadsDone = Output(Bool())
    })

    // FSM states
    val sIdle :: sDecode :: sWriteAddr :: sSetFlags :: sDone :: Nil = Enum(5)
    val state = RegInit(sIdle)

    // Registers
    val inputLoaded = RegInit(false.B)
    val outputLoaded = RegInit(false.B)
    val isOutput = RegInit(false.B)
    val loadInputAddr = RegInit(0.U(32.W))
    val loadOutputAddr = RegInit(0.U(32.W))
    val startLoad = RegInit(false.B)
    val matrixLoadsDone = RegInit(false.B)
    val bufAddr = RegInit(0.U(32.W))

    val count = RegInit(0.U(8.W))

    // Instruction alias
    val inst = io.cmd.bits

    printf(p"Cycle: $count, State: $state, isOutput: $isOutput, startLoad: $startLoad, inputLoaded: $inputLoaded, outputLoaded = $outputLoaded, loadInputAddr = $loadInputAddr, loadOutputAddr = $loadOutputAddr, matrixLoadsDone = $matrixLoadsDone\n")

    count := count + 1.U
    matrixLoadsDone := RegNext(inputLoaded && outputLoaded, init = false.B)

    // Outputs
    io.cmd.ready := ((state === sIdle) || 
                     (state === sDone && !(inputLoaded && outputLoaded)) && io.cmd.valid)

    io.busy := false.B // using cmd.ready gating to control two-instruction FSM. 

    switch(state) {
        is(sIdle) {
            when(io.cmd.valid) {             
                state := sDecode
            }
        }

        is(sDecode) {
            when(inst.opcode === "b0001011".U &&
                 inst.funct3 === "b011".U &&
                 inst.funct7 === "b0000001".U) {
                isOutput := (inst.rs1 === 1.U)  // <-- rs1 == 1 means output
                bufAddr := inst.rs2
                state := sWriteAddr
            }.otherwise {
                state := sIdle // Unknown instruction
            }
        }

        is(sWriteAddr) {
            when (isOutput) {
                loadOutputAddr := bufAddr
            }.otherwise {
                loadInputAddr := bufAddr
            }
            state := sSetFlags
        }

        is(sSetFlags) {
            startLoad := true.B 
            when(isOutput) {
                outputLoaded := true.B
            }.otherwise {
                inputLoaded := true.B 
            }
            state := sDone
        }

        is(sDone) {
            when(!(inputLoaded && outputLoaded) && io.cmd.valid) {
                state := sDecode
                startLoad := false.B 
            }.elsewhen(inputLoaded && outputLoaded) {
                state := sIdle
            }
        }
    }

    // Bind internal signals to io for testing
    io.test_startLoad       := startLoad
    io.test_isOutput        := isOutput
    io.test_loadInputAddr   := loadInputAddr
    io.test_loadOutputAddr  := loadOutputAddr
    io.test_matrixLoadsDone := matrixLoadsDone
}


test(new MatrixAddrLoader) { c =>
    // Helper to send a doLoadMatrix instruction
    def sendLoadMatrix(rs1: Int, rs2: Int): Unit = {
        c.io.cmd.valid.poke(true.B)
        c.io.cmd.bits.opcode.poke("b0001011".U)
        c.io.cmd.bits.funct3.poke("b011".U)
        c.io.cmd.bits.funct7.poke("b0000001".U)
        c.io.cmd.bits.rs1.poke(rs1.U)
        c.io.cmd.bits.rs2.poke(rs2.U)
        c.io.cmd.bits.rd.poke(0.U)
        c.clock.step(1)
        c.io.cmd.valid.poke(false.B)
    }

    print("Initial\n")
    c.clock.step(1)
    c.io.test_matrixLoadsDone.expect(false.B)
    c.io.test_startLoad.expect(false.B)

    // Wait for accelerator to be ready
    while (!c.io.cmd.ready.peek().litToBoolean) {
        c.clock.step(1)
    }

    /*
    Test 0: Load input -> load output
    Test 1: Load output -> load input
    Test 2: Load input -> load input -> load output    TODO
    Test 3: Load output -> load output -> load input   TODO
    */
    val test_num = 1

    if (test_num == 0) {
        // Load input matrix (rs1 = 0)
        print("Send load input matrix instruction\n")
        sendLoadMatrix(0, 8)

        while (!c.io.cmd.ready.peek().litToBoolean) {
            c.clock.step(1)
        }

        c.io.test_startLoad.expect(true.B)
        c.io.test_isOutput.expect(false.B) // input
        c.io.test_loadInputAddr.expect(8.U)
        c.io.test_loadOutputAddr.expect(0.U)

        // Load output matrix (rs1 = 1)
        print("Send load output matrix instruction\n")
        sendLoadMatrix(1, 16)

        while (!c.io.cmd.ready.peek().litToBoolean) {
            c.clock.step(1)
        }

        c.io.test_startLoad.expect(true.B)
        c.io.test_isOutput.expect(true.B)
        c.io.test_loadInputAddr.expect(8.U)
        c.io.test_loadOutputAddr.expect(16.U)

        c.clock.step(1)
        c.io.test_matrixLoadsDone.expect(true.B)

    } else if (test_num == 1) {
        // Load output matrix (rs1 = 1)
        print("Send load output matrix instruction\n")
        sendLoadMatrix(1, 8)

        while (!c.io.cmd.ready.peek().litToBoolean) {
            c.clock.step(1)
        }

        c.io.test_startLoad.expect(true.B)
        c.io.test_isOutput.expect(true.B)
        c.io.test_loadInputAddr.expect(0.U)
        c.io.test_loadOutputAddr.expect(8.U)

        // Load input matrix (rs1 = 0)
        print("Send load input matrix instruction\n")
        sendLoadMatrix(0, 16)

        while (!c.io.cmd.ready.peek().litToBoolean) {
            c.clock.step(1)
        }

        c.io.test_startLoad.expect(true.B)
        c.io.test_isOutput.expect(false.B)
        c.io.test_loadInputAddr.expect(16.U)
        c.io.test_loadOutputAddr.expect(8.U)

        c.clock.step(1)
        c.io.test_matrixLoadsDone.expect(true.B)
    }
}
