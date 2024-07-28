package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager.r

class Add(
    nibbles: ByteArray
) : Instruction(nibbles) {

    private lateinit var rx: R
    private lateinit var ry: R
    private lateinit var rz: R

    override fun processNibbles() {
        val rxIndex = nibbles[0].toInt()
        val ryIndex = nibbles[1].toInt()
        val rzIndex = nibbles[2].toInt()

        rx = r[rxIndex]
        ry = r[ryIndex]
        rz = r[rzIndex]
    }

    override fun performOperation() {
        val rxValue = rx.read()[0].toInt()
        val ryValue = ry.read()[0].toInt()

        val result = (rxValue + ryValue).toByte()

        rz.write(byteArrayOf(result))
    }
}
