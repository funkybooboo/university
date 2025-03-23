package com.natestott.emulator.computer.memory.contiguous

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

object RamManager {
    val ram = Ram()
}

class Ram : ContiguousMemory(
    ByteArray(4096)
) {
    override fun read(address: Int): Byte {
        val byte = bytes[address]
        logger.log(Level.INFO, "Reading from RAM address $address: ${byte.toUByte()}")
        return byte
    }

    override fun write(address: Int, byte: Byte) {
        logger.log(Level.INFO, "Writing to RAM address $address: ${byte.toUByte()}")
        bytes[address] = byte
    }
}
