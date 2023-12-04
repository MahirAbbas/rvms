package projectname

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.EntryPoint


case class Control() extends Component {
    val io = new Bundle {
        val opcode = in Bits(7 bits)
        val branch = out Bool()
        val memRead = out Bool()
        val memToReg = out Bool()
        val memWrite = out Bool()
        val aluSrc = out Bool()
        val aluOp = out Bits(3 bits)
        val regWrite = out Bool()
        val regDest = out Bool()
    }
    
    val fsm = new StateMachine {
        val s0Fetch = new State with EntryPoint
        val s1Decode = new State
        val s2MemAdr = new State
        val s3MemRead = new State
        val s4MemWB = new State
        val s5MemWrite = new State
        val s6ExecuteR = new State
        val s7ALUWB = new State
        val s8Ecevutel = new State
    }

    
    

}