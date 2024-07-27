package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.TManager.t

class SetT(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val highNibble = nibbles[0].toInt()
        val lowNibble = nibbles[1].toInt()

        val value = (highNibble shl 4) or lowNibble

        val valueByteArray = byteArrayOf(value.toByte())

        t.write(valueByteArray)
    }
}