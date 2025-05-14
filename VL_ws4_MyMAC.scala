package MAC

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._

class MyMAC(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new MyMACModuleImp(this)
}

class MyMACModuleImp(outer: MyMAC)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

        // FSM states and registers
        //val regfile = Mem(outer.n,UInt(xLen.W))
        val regfile = RegInit(VecInit(Seq.fill(5)(0.U(32.W))))
        val result = RegInit(0.U(32.W))

        val idle :: loadA :: compute :: done :: Nil = Enum(4)
        val state = RegInit(idle)
        
        
        // core.cmd signals

        val cmd = Queue(io.cmd,2) 
        val funct = cmd.bits.inst.funct
        val doWrite = funct === 0.U
        val doMAC = funct === 1.U

        cmd.ready := state === idle || state === loadA || state === compute



        // response interface
        io.resp.valid := state === done
        io.resp.bits.rd := cmd.bits.inst.rd
        io.resp.bits.data := result
        io.busy := (state === done)
 

        // connect memory interface
        io.mem.req.valid := false.B
        //io.mem.req.bits.addr := cmd.bits.rs1
        //io.mem.req.bits.cmd := M_XRD
        //io.mem.req.bits.tag := cmd.bits.inst.rs2(log2Up(5)-1,0)
        //io.mem.req.bits.data := 0.U
        //io.mem.req.bits.phys := false.B
        //io.mem.req.bits.size := log2Ceil(8).U
        //io.mem.req.bits.signed := false.B
        //io.mem.req.bits.dprv := cmd.bits.status.dprv
        


        // FSM for MAC

        when (state === idle){
            when (io.cmd.fire()){
                when (doWrite){
                    regfile(4) := 0.U // clear result register
                    regfile(0) := cmd.bits.rs1 // a0
                    regfile(1) := cmd.bits.rs2 // a1
                    state := loadA
                }
            }
        }.elsewhen (state === loadA){
            when (io.cmd.fire()){
            	printf(p"${doWrite}\n")
            	printf(p"${doMAC}\n")
                when (doWrite){
                    regfile(2) := cmd.bits.rs1 // b0
                    regfile(3) := cmd.bits.rs2 // b1
                    state := compute
                }
            }
        }.elsewhen (state === compute){
            when (io.cmd.fire()){
            	printf(p"yeet\n")
                printf(p"${funct}\n")
                printf(p"funct: ${cmd.bits.inst.funct}\n")
            	printf(p"${doWrite}\n")
            	printf(p"${doMAC}\n")
                //when (doMAC){
                printf(p"4\n")
                result := (regfile(0) * regfile(2)) + (regfile(1) * regfile(3))
                regfile(4) := result // store result in regfile[4]
                state := done
                //}
            }
        }.elsewhen (state === done){
        	printf(p"5\n")
            when (io.resp.fire()){
                state := idle
            }
        }
        
    }