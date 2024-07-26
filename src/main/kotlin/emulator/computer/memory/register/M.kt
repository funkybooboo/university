package com.natestott.emulator.computer.memory.register

object MManager {
    val m = M()
}

class M(): Register(
    ByteArray(1)
) {
    override fun read(): ByteArray {
        TODO("Not yet implemented")
    }

    override fun write(bytes: ByteArray) {
        TODO("Not yet implemented")
    }

}