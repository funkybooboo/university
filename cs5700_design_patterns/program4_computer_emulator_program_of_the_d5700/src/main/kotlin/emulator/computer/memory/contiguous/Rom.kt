package com.natestott.emulator.computer.memory.contiguous

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

object RomManager {
    private var rom: Rom? = null

    fun initializeRom(bytes: ByteArray) {
        rom = Rom(bytes)
        logger.log(Level.INFO, "ROM initialized with ${bytes.size} bytes")
    }

    fun getRom(): Rom? {
        return rom
    }
}

class Rom(
    bytes: ByteArray
) : ContiguousMemory(
    bytes
) {
    override fun read(address: Int): Byte {
        val byte = bytes[address]
        logger.log(Level.INFO, "Reading from ROM address $address: ${byte.toUByte()}")
        return byte
    }

    override fun write(address: Int, byte: Byte) {
        logger.log(Level.WARNING, "Attempted to write to ROM address $address: ${byte.toUByte()} - Operation not supported")
        throw UnsupportedOperationException("Cannot write to ROM")
    }
}
