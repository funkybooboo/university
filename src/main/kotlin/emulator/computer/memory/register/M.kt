package com.natestott.emulator.computer.memory.register

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

object MManager {
    val m = M()
}

class M : Register(
    ByteArray(1)
) {
    override fun write(bytes: ByteArray) {
        require(bytes.size == 1) { "ByteArray must be of size 1." }
        val flag = bytes[0].toInt() and 0xFF
        require(flag == 0 || flag == 1) { "Invalid flag value. Must be 0 or 1." }

        logger.log(Level.INFO, "Writing to register M: ${bytes[0].toUByte()}")

        bytes.copyInto(destination = this.bytes, startIndex = 0, endIndex = 1)

        logger.log(Level.INFO, "Register M after write: ${this.bytes[0].toUByte()}")
    }
}
