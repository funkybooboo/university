package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager.r

class Sub(
    nibbles: ByteArray
) : Instruction(nibbles) {
    lateinit var rx: R
    lateinit var ry: R
    lateinit var rz: R

    public override fun processNibbles() {
        val rxIndex = nibbles[0].toInt()
        val ryIndex = nibbles[1].toInt()
        val rzIndex = nibbles[2].toInt()

        rx = r[rxIndex]
        ry = r[ryIndex]
        rz = r[rzIndex]
    }

    public override fun performOperation() {
        val rxValue = rx.read()[0].toInt()
        val ryValue = ry.read()[0].toInt()

        val result = (rxValue - ryValue).toByte()

        rz.write(byteArrayOf(result))
    }
}
