package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.computer.memory.register.TManager.t

class ReadT(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]
        val tValue = t.read()[0]
        rx.write(byteArrayOf(tValue))
    }
}