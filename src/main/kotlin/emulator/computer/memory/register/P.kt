package com.natestott.emulator.computer.memory.register

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level
import com.natestott.emulator.computer.byteArrayToInt

object PManager {
    val p = P()
}

class P : Register(
    ByteArray(2)
) {
    override fun write(bytes: ByteArray) {
        require(bytes.size == 2) { "ByteArray must be of size 2." }
        val intValue = byteArrayToInt(bytes)
        logger.log(Level.INFO, "Attempting to write to register P: ${bytes.joinToString(", ")} (intValue=$intValue)")
        require(intValue % 2 == 0) { "Integer value must be divisible by 2." }
        bytes.copyInto(destination = this.bytes, startIndex = 0, endIndex = 2)
        logger.log(Level.INFO, "Register P after write: ${this.bytes.joinToString(", ")} (intValue=$intValue)")
    }
}
