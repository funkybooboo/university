package com.natestott.emulator.computer

import com.natestott.emulator.computer.instruction.InstructionFactory
import com.natestott.emulator.computer.memory.contiguous.Rom
import com.natestott.emulator.computer.memory.register.PManager
import com.natestott.emulator.computer.memory.register.TManager.t
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

class Cpu(
    private val instructionSpeed: Long = 2L,
    private val timerSpeed: Long = 16L
) {
    private val executor = Executors.newSingleThreadScheduledExecutor()
    private val instructionFactory = InstructionFactory()
    private var rom: Rom? = null

    private val cpuRunnable = Runnable {
        try {
            val bytes = readNextInstructionBytes()
            if (bytes[0].toInt() == 0 && bytes[1].toInt() == 0) {
                logger.log(Level.INFO, "End of program detected. Shutting down executor.")
                executor.shutdown()
                return@Runnable
            }
            val instruction = instructionFactory.createInstruction(bytes)
            instruction.execute()
        } catch (e: Exception) {
            logger.log(Level.ERROR, "Exception during instruction execution: ${e.message}", e)
            executor.shutdown()
            return@Runnable
        }
    }

    private val timerRunnable = Runnable {
        try {
            val currentT = t.read()[0].toInt()
            if (currentT > 0) {
                t.write(byteArrayOf((currentT - 1).toByte()))
            }
        } catch (e: Exception) {
            logger.log(Level.ERROR, "Exception during timer update: ${e.message}", e)
        }
    }

    fun executeProgram(rom: Rom) {
        this.rom = rom
        logger.log(Level.INFO, "Starting program execution")

        val cpuFuture = executor.scheduleAtFixedRate(
            cpuRunnable,
            0,
            instructionSpeed,
            TimeUnit.MILLISECONDS
        )

        val timerFuture = executor.scheduleAtFixedRate(
            timerRunnable,
            0,
            timerSpeed,
            TimeUnit.MILLISECONDS
        )

        try {
            cpuFuture.get()
            timerFuture.get()
        } catch (e: Exception) {
            logger.log(Level.ERROR, "Exception during execution scheduling: ${e.message}", e)
        } finally {
            executor.shutdown()
            logger.log(Level.INFO, "Executor service shut down")
        }
    }

    private fun readNextInstructionBytes(): ByteArray {
        return try {
            val p = PManager.p.read()
            val address1 = byteArrayToInt(p)
            val address2 = byteArrayToInt(p + 1)
            val byte1 = rom?.read(address1) ?: 0
            val byte2 = rom?.read(address2) ?: 0
            byteArrayOf(byte1, byte2)
        } catch (e: Exception) {
            logger.log(Level.ERROR, "Exception while reading next instruction bytes: ${e.message}", e)
            byteArrayOf(0, 0)
        }
    }
}