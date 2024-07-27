package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.computer.memory.register.PManager.p

class SkipEqual(
    nibbles: ByteArray
) : Instruction(nibbles) {

    private var shouldSkip = false

    override fun performOperation() {

        val rxIndex = nibbles[0].toInt()
        val ryIndex = nibbles[1].toInt()

        val rxValue = r[rxIndex].read()[0].toInt()
        val ryValue = r[ryIndex].read()[0].toInt()

        shouldSkip = (rxValue == ryValue)
    }

    override fun incrementProgramCounter() {
        if (shouldSkip) {
            val currentPC = byteArrayToInt(p.read())
            val newPC = currentPC + 4
            p.write(intToByteArray(newPC))
        } else {
            val currentPC = byteArrayToInt(p.read())
            val newPC = currentPC + 2
            p.write(intToByteArray(newPC))
        }
    }
}
