package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.ScreenManager.screen
import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager.r

class Draw(
    nibbles: ByteArray
) : Instruction(nibbles) {

    private lateinit var rx: R
    private var row: Byte = 0
    private var col: Byte = 0

    override fun processNibbles() {
        val rxIndex = nibbles[0].toInt()
        rx = r[rxIndex]
        row = nibbles[1]
        col = nibbles[2]
    }

    override fun performOperation() {
        val asciiValue = rx.read()[0].toInt()

        if (asciiValue > 0x7F) {
            throw IllegalArgumentException("ASCII value in rX is greater than 0x7F.")
        }

        screen.draw(asciiValue.toByte(), row, col)
    }
}
