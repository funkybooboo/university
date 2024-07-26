package com.natestott.emulator.computer.memory.register

object TManager {
    val t = T()
}

class T(): Register(
    ByteArray(1)
) {
    override fun read(): ByteArray {
        TODO("Not yet implemented")
    }

    override fun write(bytes: ByteArray) {
        TODO("Not yet implemented")
    }

}