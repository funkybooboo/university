package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class ConvertByteToAscii(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val ryIndex = nibbles[1].toInt()

        val rx = r[rxIndex]
        val ry = r[ryIndex]

        val value = rx.read()[0].toInt()

        require(value <= 0xF) {"Value in rX is out of range (0-F)."}

        val asciiValue = if (value < 10) {
            (value + '0'.code).toByte()
        } else {
            (value - 10 + 'A'.code).toByte()
        }

        ry.write(byteArrayOf(asciiValue))

        logger.log(Level.INFO, "Converted value $value to ASCII character ${asciiValue.toInt().toChar()} and stored in rY.")
    }
}
