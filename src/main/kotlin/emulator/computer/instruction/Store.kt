package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.combineNibblesToByte
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class Store(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        val highNibble = nibbles[1]
        val lowNibble = nibbles[2]

        val byte = combineNibblesToByte(highNibble, lowNibble)

        logger.log(Level.INFO, "Performing Store Operation:")
        logger.log(Level.INFO, "Register R$rxIndex before store operation: ${rx.read()[0].toUByte()}")
        logger.log(Level.INFO, "High nibble: ${highNibble.toUByte()}")
        logger.log(Level.INFO, "Low nibble: ${lowNibble.toUByte()}")
        logger.log(Level.INFO, "Combined byte: ${byte.toUByte()}")

        rx.write(byteArrayOf(byte))

        logger.log(Level.INFO, "Register R$rxIndex after store operation: ${rx.read()[0].toUByte()}")
    }
}
