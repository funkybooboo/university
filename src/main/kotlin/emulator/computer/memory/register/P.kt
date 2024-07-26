package com.natestott.emulator.computer.memory.register

object PManager {
    val p = P()
}

class P(): Register(
    ByteArray(2)
) {
    override fun read(): ByteArray {
        TODO("Not yet implemented")
    }

    override fun write(bytes: ByteArray) {
        TODO("Not yet implemented")
    }

}