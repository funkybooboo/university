package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.MManager.m
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class SwitchMemory(
    nibbles: ByteArray
) : Instruction(nibbles) {

    public override fun processNibbles() {
        // Nothing to process.
    }

    public override fun performOperation() {
        val currentMValue = m.read()[0].toInt()
        val newMValue = if (currentMValue == 0) {
            1
        } else {
            0
        }
        val newMValueBytes = byteArrayOf(newMValue.toByte())

        logger.log(Level.INFO, "Switching memory mode:")
        logger.log(Level.INFO, "Current M register value: $currentMValue")
        logger.log(Level.INFO, "New M register value: $newMValue")

        m.write(newMValueBytes)
    }
}
