package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.AManager.a
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class SetA(
    nibbles: ByteArray
) : Instruction(nibbles) {
    private lateinit var addressBytes: ByteArray

    override fun processNibbles() {
        val highNibble = nibbles[0].toInt()
        val middleNibble = nibbles[1].toInt()
        val lowNibble = nibbles[2].toInt()

        val address = (highNibble shl 8) or (middleNibble shl 4) or lowNibble
        addressBytes = intToByteArray(address)
    }

    override fun performOperation() {
        a.write(addressBytes)

        logger.log(Level.INFO, "Performing SetA Operation:")
        logger.log(Level.INFO, "Writing address to A register: ${addressBytes.joinToString(prefix = "0x", separator = "", transform = { it.toUByte().toString(16).uppercase() })}")
    }
}
