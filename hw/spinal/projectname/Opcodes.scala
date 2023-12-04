package rvss

import spinal.core._

// LOOK AT SOME ENCODING
object OpCode extends SpinalEnum {
    val LUI,AUIPC, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU, LB, LH, LW, LBU, LHU, SB, SH, SW, ADDI, 
    SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI, ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND, FENCE, NOOP  = newElement()
}


object InstrFormat extends SpinalEnum {
    val R = newElement()
    val I = newElement()
    val S = newElement()
    val B = newElement()
    val U = newElement()
    val J = newElement()
    val UNDEF = newElement()
}

object Rvi {
    
    def ADD                = M"0000000----------000-----0110011"
    def SUB                = M"0100000----------000-----0110011"
    def SLL                = M"0000000----------001-----0110011"
    def SLT                = M"0000000----------010-----0110011"
    def SLTU               = M"0000000----------011-----0110011"
    def XOR                = M"0000000----------100-----0110011"
    def SRL                = M"0000000----------101-----0110011"
    def SRA                = M"0100000----------101-----0110011"
    def OR                 = M"0000000----------110-----0110011"
    def AND                = M"0000000----------111-----0110011" 
    
    def ADDI               = M"-----------------000-----0010011"
    def SLLI               = M"000000-----------001-----0010011"
    def SLTI               = M"-----------------010-----0010011"
    def SLTIU              = M"-----------------011-----0010011"
    def XORI               = M"-----------------100-----0010011"
    def SRLI               = M"000000-----------101-----0010011"
    def SRAI               = M"010000-----------101-----0010011"
    def ORI                = M"-----------------110-----0010011"
    def ANDI               = M"-----------------111-----0010011"

    def ADDW               = M"0000000----------000-----0111011"
    def SUBW               = M"0100000----------000-----0111011"

    def LB                 = M"-----------------000-----0000011"
    def LH                 = M"-----------------001-----0000011"
    def LW                 = M"-----------------010-----0000011"
    def LD                 = M"-----------------011-----0000011"
    def LBU                = M"-----------------100-----0000011"
    def LHU                = M"-----------------101-----0000011"
    def LWU                = M"-----------------110-----0000011"

    def SB                 = M"-----------------000-----0100011"
    def SH                 = M"-----------------001-----0100011"
    def SW                 = M"-----------------010-----0100011"
    def SD                 = M"-----------------011-----0100011"
}
