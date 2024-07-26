package com.natestott.emulator.computer.memory.contiguous

class Rom(
    bytes: ByteArray
): ContiguousMemory(
    bytes
) {
    override fun read(): Byte {
        TODO("Not yet implemented")
    }

    override fun write(byte: Byte) {
        TODO("Not yet implemented")
    }
}