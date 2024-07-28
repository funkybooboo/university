package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager.r

class ConvertByteToAscii(
    nibbles: ByteArray
) : Instruction(nibbles) {

    private lateinit var rx: R
    private lateinit var ry: R

    override fun processNibbles() {
        val rxIndex = nibbles[0].toInt()
        val ryIndex = nibbles[1].toInt()

        rx = r[rxIndex]
        ry = r[ryIndex]
    }

    override fun performOperation() {
        val value = rx.read()[0].toInt()

        require(value <= 0xF) {"Value in rX is out of range (0-F)."}

        val asciiValue = if (value < 10) {
            (value + '0'.code).toByte()
        } else {
            (value - 10 + 'A'.code).toByte()
        }

        ry.write(byteArrayOf(asciiValue))
    }
}
