package com.natestott.emulator.computer.memory.register

class AManager() {
    val a = A()
}

class A(): Register(
    ByteArray(2)
) {
    override fun read(): ByteArray {
        TODO("Not yet implemented")
    }

    override fun write(bytes: ByteArray) {
        TODO("Not yet implemented")
    }

}