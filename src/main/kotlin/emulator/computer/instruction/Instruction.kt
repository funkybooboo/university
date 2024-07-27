package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.PManager.p
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

abstract class Instruction(
    protected val nibbles: ByteArray
) {
    init {
        require(nibbles.size == 3) { "Nibbles array must contain exactly 3 elements." }
        logger.log(Level.INFO, "Instruction initialized with nibbles: ${nibbles.joinToString(", ")}")
    }

    fun execute() {
        logger.log(Level.INFO, "Executing instruction with nibbles: ${nibbles.joinToString(", ")}")
        performOperation()
        incrementProgramCounter()
    }

    protected open fun incrementProgramCounter() {
        val currentPC = byteArrayToInt(p.read())
        val newPC = currentPC + 2
        p.write(intToByteArray(newPC))
        logger.log(Level.INFO, "Program Counter incremented from $currentPC to $newPC")
    }

    protected abstract fun performOperation()
}
