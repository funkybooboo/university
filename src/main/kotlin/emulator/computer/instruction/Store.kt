package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.combineNibblesToByte
import com.natestott.emulator.computer.memory.register.RManager.r

class Store(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        val highNibble = nibbles[1]
        val lowNibble = nibbles[2]

        val byte = combineNibblesToByte(highNibble, lowNibble)

        rx.write(byteArrayOf(byte))
    }
}