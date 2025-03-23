package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.combineNibblesToByte
import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class Store(
    nibbles: ByteArray
) : Instruction(nibbles) {

    lateinit var rx: R
    var byte: Byte = 0

    public override fun processNibbles() {
        val rxIndex = nibbles[0].toInt()
        rx = r[rxIndex]

        val highNibble = nibbles[1]
        val lowNibble = nibbles[2]

        byte = combineNibblesToByte(highNibble, lowNibble)
    }

    public override fun performOperation() {
        logger.log(Level.INFO, "Performing Store Operation:")
        logger.log(Level.INFO, "Combined byte: ${byte.toUByte()}")

        rx.write(byteArrayOf(byte))
    }
}
