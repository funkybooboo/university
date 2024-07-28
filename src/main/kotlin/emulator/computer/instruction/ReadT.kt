package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.PauseTimerManager
import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.computer.memory.register.TManager.t
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class ReadT(
    nibbles: ByteArray
) : Instruction(nibbles) {

    private lateinit var rx: R

    override fun processNibbles() {
        val rxIndex = nibbles[0].toInt()
        rx = r[rxIndex]
    }

    override fun performOperation() {
        PauseTimerManager.pauseTimer.set(true)

        val tValue = t.read()[0]

        rx.write(byteArrayOf(tValue))

        logger.log(Level.INFO, "Performing ReadT Operation:")
        logger.log(Level.INFO, "Value read from T register: ${tValue.toInt()} (0x${tValue.toUByte().toString(16).uppercase()})")

        PauseTimerManager.pauseTimer.set(false)
    }
}
