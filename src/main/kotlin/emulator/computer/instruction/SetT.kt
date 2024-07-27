package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.TManager.t
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class SetT(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        val highNibble = nibbles[0].toInt()
        val lowNibble = nibbles[1].toInt()

        val value = (highNibble shl 4) or lowNibble
        val valueByteArray = byteArrayOf(value.toByte())

        t.write(valueByteArray)

        logger.log(Level.INFO, "Performing SetT Operation:")
        logger.log(Level.INFO, "High nibble: $highNibble")
        logger.log(Level.INFO, "Low nibble: $lowNibble")
        logger.log(Level.INFO, "Combined value: $value (0x${value.toString(16).uppercase()})")
        logger.log(Level.INFO, "Writing value to T register: ${valueByteArray[0].toInt()}")
    }
}
