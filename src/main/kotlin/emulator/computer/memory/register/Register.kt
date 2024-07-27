package com.natestott.emulator.computer.memory.register

import com.natestott.emulator.computer.memory.Memory
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

abstract class Register(
    bytes: ByteArray
): Memory(bytes) {
    fun read(): ByteArray {
        val readBytes = bytes.copyOf()
        logger.log(Level.INFO, "Reading from register: ${readBytes.joinToString(", ")}")
        return readBytes
    }

    abstract fun write(bytes: ByteArray)
}