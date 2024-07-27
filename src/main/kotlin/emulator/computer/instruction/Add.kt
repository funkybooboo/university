package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class Add(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val ryIndex = nibbles[1].toInt()
        val rzIndex = nibbles[2].toInt()

        val rx = r[rxIndex]
        val ry = r[ryIndex]
        val rz = r[rzIndex]

        val rxValue = rx.read()[0].toInt()
        val ryValue = ry.read()[0].toInt()

        val result = (rxValue + ryValue).toByte()

        rz.write(byteArrayOf(result))

        logger.log(Level.INFO, "Added values from r$rxIndex and r$ryIndex. Result: $result stored in r$rzIndex.")
    }
}
