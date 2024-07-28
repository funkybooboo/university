package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.PauseTimerManager
import com.natestott.emulator.computer.combineNibblesToByte
import com.natestott.emulator.computer.memory.register.TManager.t
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class SetT(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        PauseTimerManager.pauseTimer.set(true)

        val highNibble = nibbles[0]
        val lowNibble = nibbles[1]

        val value = combineNibblesToByte(highNibble, lowNibble)

        t.write(byteArrayOf(value))

        logger.log(Level.INFO, "Performing SetT Operation:")
        logger.log(Level.INFO, "High nibble: $highNibble")
        logger.log(Level.INFO, "Low nibble: $lowNibble")
        logger.log(Level.INFO, "Combined value: $value (0x${value.toString(16).uppercase()})")
        logger.log(Level.INFO, "Writing value to T register: ${value.toInt()}")

        PauseTimerManager.pauseTimer.set(false)
    }
}
