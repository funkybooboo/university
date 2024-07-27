package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class Sub(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        val ryIndex = nibbles[1].toInt()
        val ry = r[ryIndex]

        val rzIndex = nibbles[2].toInt()
        val rz = r[rzIndex]

        val int1 = byteArrayToInt(rx.read())
        val int2 = byteArrayToInt(ry.read())
        val result = int1 - int2
        val resultBytes = intToByteArray(result)

        logger.log(Level.INFO, "Performing Subtraction Operation:")
        logger.log(Level.INFO, "Subtracting value in R$ryIndex (${int2.toUShort()}) from value in R$rxIndex (${int1.toUShort()})")
        logger.log(Level.INFO, "Result of subtraction: $result")
        logger.log(Level.INFO, "Storing result in R$rzIndex")

        rz.write(resultBytes)
    }
}
