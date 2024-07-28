package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.computer.memory.register.PManager.p
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

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

        logger.log(Level.INFO, "Performing SkipEqual Operation:")
        logger.log(Level.INFO, "Comparing R$rxIndex ($rxValue) and R$ryIndex ($ryValue)")
        logger.log(Level.INFO, "Comparison result: ${if (shouldSkip) "Equal (will skip)" else "Not Equal (will not skip)"}")
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
