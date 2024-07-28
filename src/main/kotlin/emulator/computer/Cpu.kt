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
            require(bytes.size == 2) { "ByteArray must contain exactly 2 bytes." }
            if (bytes[0].toInt() == 0 && bytes[1].toInt() == 0) {
                logger.log(Level.INFO, "End of program detected. Shutting down executor.")
                executor.shutdown()
                return@Runnable
            }

            logger.log(Level.INFO, "Retrieved next instruction: ${bytes.joinToString(", ")}")
            val nibbles01 = breakByteIntoNibbles(bytes[0])
            val nibbles23 = breakByteIntoNibbles(bytes[1])
            val nibble0 = nibbles01.first
            val nibble1 = nibbles01.second
            val nibble2 = nibbles23.first
            val nibble3 = nibbles23.second
            logger.log(Level.INFO, "Parsed nibbles: $nibble0, $nibble1, $nibble2, $nibble3")

            val instruction = instructionFactory.createInstruction(nibble0, nibble1, nibble2, nibble3)
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
            val pc = byteArrayToInt(p)
            val byte1 = rom?.read(pc) ?: 0
            val byte2 = rom?.read(pc + 1) ?: 0
            byteArrayOf(byte1, byte2)
        } catch (e: Exception) {
            logger.log(Level.ERROR, "Exception while reading next instruction bytes: ${e.message}", e)
            byteArrayOf(0, 0)
        }
    }
}