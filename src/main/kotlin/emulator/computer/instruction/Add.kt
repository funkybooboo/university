package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.RManager.r

class Add(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        val ryIndex = nibbles[1].toInt()
        val ry = r[ryIndex]

        val rzIndex = nibbles[2].toInt()
        val rz = r[rzIndex]

        rz.write(rx.read() + ry.read())
    }
}