package rvss

import spinal.core._
import spinal.lib._

case class InstrMemory() extends Component {
    val io = new Bundle {
        val readData = out Bits(32 bits)
        val writeEnable = in Bool()
        val writeData = in Bits(32 bits)
        val address = in UInt(32 bits)
    }
    val instrMemory = Mem(SInt(32 bits), 256)
    
    io.readData := instrMemory.readSync(io.address).asBits

    when(io.writeEnable) {
        instrMemory.write(io.address, io.writeData.asSInt)
    }  
    





}