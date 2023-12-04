package projectname

import spinal.core._
import spinal.lib._
import rvss._


case class Extend() extends Component {
    val io = new Bundle {
        val instr = in Bits(32 bits)
        val instrFormat = in(InstrFormat())
        val immediate = out(SInt(32 bits))
    }

    val i_imm = io.instr(31 downto 20) 
    val s_imm = io.instr(31 downto 25) ## io.instr(11 downto 7)
    val b_imm = io.instr(31) ## io.instr(7) ## io.instr(30 downto 25) ## io.instr(11 downto 8) ## B("0")
    val u_imm = io.instr(31 downto 12)
    val j_imm = io.instr(31) ## io.instr(19 downto 12) ## io.instr(20) ## io.instr(30 downto 21) ## B("0")
    
    switch(io.instrFormat) {
        is(InstrFormat.I) {io.immediate := i_imm.asSInt.resize(32 bits)}
        is(InstrFormat.S) {io.immediate := s_imm.asSInt.resize(32 bits)}
        is(InstrFormat.B) {io.immediate := b_imm.asSInt.resize(32 bits)}
        is(InstrFormat.U) {io.immediate := u_imm.asSInt.resize(32 bits)}
        is(InstrFormat.J) {io.immediate := j_imm.asSInt.resize(32 bits)}
    }

}

case class Datapath() extends Component {
    val io = new Bundle {
        val instr = in Bits(32 bits)
        val IRWriteEnable = in Bool()
        val PC = in UInt(32 bits)
        val PCWRite = in Bool()
        val AdrSrc = in Bool()
        val MemWrite = in Bool()
        val ResultSrc = in Bits(3 bits)
        val ALUControl = in Bits(4 bits)
        val ALUSrcA = in Bits(2 bits)
        val ALUSrcB = in Bits(2 bits)
        val regFileWriteEnable = in Bool()
    }
    
    
    val instrMemory = new InstrMemory()
    val regFile = new RegFile()
    val alu = new ALU()
    val instructionRegister = Reg(Bits(32 bits)) init 0
    val extend = new Extend() 
    
    when (io.IRWriteEnable === True) {
        instructionRegister := instrMemory.io.readData 
    }
    
    regFile.io.readAddress1 := instructionRegister(19 downto 15).asUInt
    regFile.io.readAddress2 := instructionRegister(24 downto 20).asUInt

    val counter = Counter(0 to 63)
    counter.increment()
    val pcAddress = counter * 4

    
    val i_imm = io.instr(31 downto 20) 
    val s_imm = io.instr(31 downto 25) ## io.instr(11 downto 7)
    val b_imm = io.instr(31) ## io.instr(7) ## io.instr(30 downto 25) ## io.instr(11 downto 8) ## B("0")
    val u_imm = io.instr(31 downto 12)
    val j_imm = io.instr(31) ## io.instr(19 downto 12) ## io.instr(20) ## io.instr(30 downto 21) ## B("0")
    
    val aluSrcARegister = Reg(SInt(32 bits)) init 0
    aluSrcARegister := regFile.io.readData1.asSInt
    alu.io.SrcA := io.ALUSrcA.mux(
        0 -> io.PC,
        1 -> UNDEF,
        2 -> aluSrcARegister
    )      
    alu.io.SrcB := io.ALUSrcB.mux(
        0 -> UNDEF,
        1 -> extend.io.immediate,
        2 -> UNDEF

    )  
    val ALUResultRegister = Reg(Bits(32 bits))    
    ALUResultRegister := alu.io.ALUResult
    instrMemory.io.address := Mux(io.AdrSrc, ALUResultRegister.asUInt, io.PC)

    val dataRegister = Reg(Bits(32 bits)) init 0
    dataRegister := instrMemory.io.readData
    

    val result = io.ResultSrc.mux(
        0 -> ALUResultRegister,
        1 -> dataRegister,
        2 -> alu.io.ALUResult
    )

    instrMemory.io.writeData := UNDEF 
    
    




}