package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.RManager.r

class ReadKeyboard(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        println("Enter up to 2 hexadecimal digits (0-F): ")
        val input = readln().trim().uppercase()

        val byte = parseHexInput(input)

        rx.write(ByteArray(1) { byte })
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