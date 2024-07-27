package com.natestott.emulator.computer.memory.contiguous

object RamManager {
    val ram = Ram()
}

class Ram(): ContiguousMemory(
    ByteArray(4096)
) {
    override fun read(address: Int): Byte {
        return bytes[address]
    }

    override fun write(address: Int, byte: Byte) {
        bytes[address] = byte
    }
}