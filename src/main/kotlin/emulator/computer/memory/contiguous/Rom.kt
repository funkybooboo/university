package com.natestott.emulator.computer.memory.contiguous

object RomManager {
    private var rom: Rom? = null

    fun initializeRom(bytes: ByteArray) {
        rom = Rom(bytes)
    }

    fun getRom(): Rom? {
        return rom
    }
}

class Rom(
    bytes: ByteArray
): ContiguousMemory(
    bytes
) {
    override fun read(address: Int): Byte {
        return bytes[address]
    }

    override fun write(address: Int, byte: Byte) {
        throw UnsupportedOperationException("Cannot write to ROM")
    }
}