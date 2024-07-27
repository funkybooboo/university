package com.natestott.emulator.computer.memory.register

object MManager {
    val m = M()
}

class M(): Register(
    ByteArray(1)
) {
    override fun write(bytes: ByteArray) {
        require(bytes.size == 1) { "ByteArray must be of size 1." }
        val flag = bytes[0].toInt() and 0xFF
        require(flag == 0 || flag == 1) { "Invalid flag value. Must be 0 or 1." }
        bytes.copyInto(destination = this.bytes, startIndex = 0, endIndex = 1)
    }
}