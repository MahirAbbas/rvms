package rvss

import spinal.core._
import OpCode._
import spinal.core.sim._


case class Decode() extends Component {
    val io = new Bundle {
        val instr = in Bits(32 bits)
        val operation = out(OpCode()) simPublic()
        val outInstr = out Bits(32 bits)
        val outInstrFormat = out(InstrFormat())
        outInstr := instr
        // val operation = out(Reg(OpCode)) simPublic()
    }
    val register =  Reg(OpCode())
    // val i_imm = io.instr(31 downto 20) 
    // val s_imm = io.instr(31 downto 25) ## io.instr(11 downto 7)
    // val b_imm = io.instr(31) ## io.instr(7) ## io.instr(30 downto 25) ## io.instr(11 downto 8) ## B("0")
    // val u_imm = io.instr(31 downto 12)
    // val j_imm = io.instr(31) ## io.instr(19 downto 12) ## io.instr(20) ## io.instr(30 downto 21) ## B("0")
    io.operation := OpCode.NOOP
    io.outInstrFormat := InstrFormat.UNDEF
    // io.outInstrFormat := InstrFormat.UNDEF

    object InstrFormat extends SpinalEnum {
        val R = newElement()
        val I = newElement()
        val S = newElement()
        val B = newElement()
        val U = newElement()
        val J = newElement()
        val UNDEF = newElement()
    }
    
    val decodeInstrBits = new Area{
        val opcode = io.instr(6 downto 0)
        val funct3 = io.instr(14 downto 12)
        val funct7 = io.instr(31 downto 25)
        // val itype = InstrFormat() 
        switch(opcode) {
            // I-Type Instructions
            // 3
            //
            is(B"0000011") {
                switch(funct3) {
                    io.outInstrFormat:= InstrFormat.I
                    is(B"000") {io.operation := OpCode.LB}
                    is(B"001") {io.operation := OpCode.LH}
                    is(B"010") {io.operation := OpCode.LW}
                    is(B"100") {io.operation := OpCode.LBU}
                    is(B"101") {io.operation := OpCode.LHU}
                }
            }
            // 19
            is(B"0010011") {
                switch(funct3) {
                    io.outInstrFormat := InstrFormat.I
                    is(B"000") {io.operation := OpCode.ADDI}
                    is(B"001") {io.operation := OpCode.SLLI}
                    is(B"010") {io.operation := OpCode.SLTI}
                    is(B"011") {io.operation := OpCode.SLTIU}
                    is(B"100") {io.operation := OpCode.XORI}
                    is(B"101") {when (funct7 === B"0000000") {io.operation := OpCode.SRLI} otherwise {io.operation := OpCode.SRAI}}
                    is(B"110") {io.operation := OpCode.ORI}
                    is(B"111") {io.operation := OpCode.ANDI}
                }
            }
            // 103 
            is(B"1100111") {
                io.outInstrFormat := InstrFormat.I
                io.operation := OpCode.JALR
            }
            
            // U-Type Instructions
            // 23
            is(B"0010111") {
                io.operation := OpCode.AUIPC
                io.outInstrFormat := InstrFormat.U
            }
            // 55
            is(B"0110111") {
                io.operation := OpCode.LUI
                io.outInstrFormat := InstrFormat.U
            }

            // S-Type Instructions
            // 35
            is(B"0100011") {
                io.outInstrFormat := InstrFormat.S
                switch(funct3) {
                    is(B"000") {io.operation := OpCode.SB}
                    is(B"001") {io.operation := OpCode.SH}
                    is(B"010") {io.operation := OpCode.SW}
                }
            }
            
            // R-Type Instructions 
            // 51
            is(B"0110011") {
                io.outInstrFormat := InstrFormat.R
                switch(funct3) {
                    is (B"000") {
                        switch(funct7) {
                            is(B"0000000") {io.operation := OpCode.ADD}
                            is(B"0100000") {io.operation := OpCode.SUB}
                        }
                    }
                    is (B"001") {io.operation := OpCode.SLL}
                    is (B"010") {io.operation := OpCode.SLT}
                    is (B"011") {io.operation := OpCode.SLTU}
                    is (B"100") {io.operation := OpCode.XOR}
                    is (B"101") {
                        switch(funct7) {
                            is(B"0000000") {io.operation := OpCode.SRL}
                            is(B"0100000") {io.operation := OpCode.SRA}
                        }
                    }
                    is (B"110") {io.operation := OpCode.OR}
                    is (B"111") {io.operation := OpCode.AND}

                }
            }

            // B-Type Instructions
            // 99
            is(B"1100011") {
                io.outInstrFormat := InstrFormat.B
                switch(funct3) {
                    is(B"000") {io.operation := OpCode.BEQ}
                    is(B"001") {io.operation := OpCode.BNE}
                    is(B"100") {io.operation := OpCode.BLT}
                    is(B"101") {io.operation := OpCode.BGE}
                    is(B"110") {io.operation := OpCode.BLTU}
                    is(B"111") {io.operation := OpCode.BGEU}
                }
            }
            // 111
            is(B"1101111") {
                io.operation := OpCode.JAL
                io.outInstrFormat := InstrFormat.J
            }
            default {io.operation := OpCode.NOOP}


        }

    }
    
    // println(testopcode.toString())

    
}