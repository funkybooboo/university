package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.RManager.r

class Sub(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        val ryIndex = nibbles[1].toInt()
        val ry = r[ryIndex]

        val rzIndex = nibbles[2].toInt()
        val rz = r[rzIndex]

        val int1 = byteArrayToInt(rx.read())

        val int2 = byteArrayToInt(ry.read())

        val bytes = intToByteArray(int1 - int2)

        rz.write(bytes)
    }
}
