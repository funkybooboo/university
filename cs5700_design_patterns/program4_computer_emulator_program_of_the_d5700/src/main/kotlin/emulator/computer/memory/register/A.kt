package com.natestott.emulator.computer.memory.register

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

object AManager {
    val a = A()
}

class A : Register(
    ByteArray(2)
) {
    override fun write(bytes: ByteArray) {
        require(bytes.size == 2) { "ByteArray must be of size 2." }
        logger.log(Level.INFO, "Writing to register A: ${bytes.joinToString(", ")}")
        bytes.copyInto(destination = this.bytes, startIndex = 0, endIndex = 2)
        logger.log(Level.INFO, "Register A after write: ${this.bytes.joinToString(", ")}")
    }
}
