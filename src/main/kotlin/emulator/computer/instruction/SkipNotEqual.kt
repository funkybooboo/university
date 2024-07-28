package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.PManager.p
import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class SkipNotEqual(
    nibbles: ByteArray
) : Instruction(nibbles) {
    private var shouldSkip = false

    private lateinit var rx: R
    private lateinit var ry: R

    override fun processNibbles() {
        val rxIndex = nibbles[0].toInt()
        val ryIndex = nibbles[1].toInt()
        rx = r[rxIndex]
        ry = r[ryIndex]
    }

    override fun performOperation() {
        val rxValue = rx.read()[0].toInt()
        val ryValue = ry.read()[0].toInt()

        shouldSkip = (rxValue != ryValue)

        logger.log(Level.INFO, "Performing SkipNotEqual Operation:")
        logger.log(Level.INFO, "Comparison result: ${if (shouldSkip) "Not Equal (will skip)" else "Equal (will not skip)"}")
    }

    override fun incrementProgramCounter() {
        val currentPC = byteArrayToInt(p.read())
        val offset = if (shouldSkip) 4 else 2
        val newPC = currentPC + offset
        p.write(intToByteArray(newPC))

        logger.log(Level.INFO, "Updating Program Counter:")
        logger.log(Level.INFO, "Current PC: $currentPC")
        logger.log(Level.INFO, "Offset: $offset")
        logger.log(Level.INFO, "New PC: $newPC")
    }
}
