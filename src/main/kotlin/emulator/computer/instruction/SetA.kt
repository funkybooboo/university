package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.AManager.a

class SetA(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val highNibble = nibbles[0].toInt()
        val middleNibble = nibbles[1].toInt()
        val lowNibble = nibbles[2].toInt()

        val address = (highNibble shl 8) or (middleNibble shl 4) or lowNibble

        val addressBytes = intToByteArray(address)

        a.write(addressBytes)
    }
}