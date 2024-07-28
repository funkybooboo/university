package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class ReadKeyboard(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        logger.log(Level.INFO, "Reading input for R$rxIndex register")

        println("Enter up to 2 hexadecimal digits (0-F): ")
        val input = readln().trim().uppercase()

        val byte = parseHexInput(input)

        rx.write(byteArrayOf(byte))

        logger.log(Level.INFO, "Input received: $input")
        logger.log(Level.INFO, "Parsed byte value: ${byte.toInt()} (0x${byte.toUByte().toString(16).uppercase()})")
        logger.log(Level.INFO, "Written to R$rxIndex register: ${byte.toInt()} (0x${byte.toUByte().toString(16).uppercase()})")
    }

    private fun parseHexInput(input: String): Byte {
        if (input.isEmpty() || !input.matches(Regex("^[0-9A-F]*$"))) {
            return 0
        }

        return try {
            val hexString = input.take(2)
            hexString.toInt(16).toByte()
        } catch (e: NumberFormatException) {
            0
        }
    }
}
