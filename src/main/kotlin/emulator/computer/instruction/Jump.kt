package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.PManager.p

class Jump(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val highNibble = nibbles[0].toInt()
        val middleNibble = nibbles[1].toInt()
        val lowNibble = nibbles[2].toInt()

        val address = (highNibble shl 8) or (middleNibble shl 4) or lowNibble

        val addressBytes = intToByteArray(address)

        p.write(addressBytes)
    }

    override fun incrementProgramCounter() {
        // Program counter is not incremented after this instruction.
    }
}