package com.natestott.emulator.computer.memory.register

object AManager{
    val a = A()
}

class A(): Register(
    ByteArray(2)
) {
    override fun write(bytes: ByteArray) {
        require(bytes.size == 2) { "ByteArray must be of size 2." }
        bytes.copyInto(destination = this.bytes, startIndex = 0, endIndex = 2)
    }
}