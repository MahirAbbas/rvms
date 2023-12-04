package rvss

import spinal.core._
import spinal.lib._
import spinal.core.sim._

case class RegFile() extends Component {
    val io = new Bundle {
        val writeEnable3 = in Bool()
        val readAddress1, readAddress2 = in UInt(5 bits)
        val readData1, readData2 = out Bits(32 bits) simPublic()

        val writeAddress3 = in UInt(5 bits)
        val writeData3 = in Bits(32 bits)
    }
    val regFile = Mem(Bits(32 bits), 32) simPublic()
    
    regFile.init(Seq.fill(32)(B(0,32 bits)))
    
    when(io.writeEnable3 && io.writeAddress3 =/= 0){
        regFile.write(address = io.writeAddress3, data = io.writeData3)
    }
    
    io.readData1:= regFile.readSync(address= io.readAddress1)
    io.readData2:= regFile.readSync(address= io.readAddress2)
    
}
