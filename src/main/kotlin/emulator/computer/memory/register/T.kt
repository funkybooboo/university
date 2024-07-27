package com.natestott.emulator.computer.memory.register

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

object TManager {
    val t = T()
}

class T : Register(
    ByteArray(1)
) {
    override fun write(bytes: ByteArray) {
        require(bytes.size == 1) { "ByteArray must be of size 1." }
        logger.log(Level.INFO, "Writing to register T: ${bytes[0].toUByte()}")
        bytes.copyInto(destination = this.bytes, startIndex = 0, endIndex = 1)
        logger.log(Level.INFO, "Register T after write: ${this.bytes[0].toUByte()}")
    }
}
