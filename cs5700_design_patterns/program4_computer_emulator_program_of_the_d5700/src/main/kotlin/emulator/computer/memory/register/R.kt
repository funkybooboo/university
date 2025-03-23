package com.natestott.emulator.computer.memory.register

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

object RManager {
    val r = arrayOf(R(),R(),R(),R(),R(),R(),R(),R())
}

class R : Register(
    ByteArray(1)
) {
    override fun write(bytes: ByteArray) {
        require(bytes.size == 1) { "ByteArray must be of size 1." }
        logger.log(Level.INFO, "Writing to register R: ${bytes[0].toUByte()}")
        bytes.copyInto(destination = this.bytes, startIndex = 0, endIndex = 1)
        logger.log(Level.INFO, "Register R after write: ${this.bytes[0].toUByte()}")
    }
}
