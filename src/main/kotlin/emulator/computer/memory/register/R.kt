package com.natestott.emulator.computer.memory.register

object RManger {
    val r = Array(8) { R() }
}

class R(): Register(
    ByteArray(1)
) {
    override fun read(): ByteArray {
        TODO("Not yet implemented")
    }

    override fun write(bytes: ByteArray) {
        TODO("Not yet implemented")
    }

}