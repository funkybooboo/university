package com.natestott.emulator.computer

import com.natestott.emulator.computer.instruction.Instruction
import com.natestott.emulator.computer.instruction.InstructionFactory
import com.natestott.emulator.computer.memory.contiguous.Rom
import com.natestott.emulator.computer.memory.register.PManager
import com.natestott.emulator.computer.memory.register.TManager.t
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

class Cpu(
    private val instructionSpeed: Long = 2L,
    private val timerSpeed: Long = 16L
) {
    private val executor = Executors.newSingleThreadScheduledExecutor()
    private val cpuRunnable = Runnable {
        val instruction = readNextInstruction()
        instruction.execute()
    }
    private val timerRunnable = Runnable {
        val currentT = t.read()[0].toInt()
        if (currentT > 0) {
            t.write(byteArrayOf((currentT - 1).toByte()))
        }
    }
    private val instructionFactory = InstructionFactory()
    private var rom: Rom? = null

    fun executeProgram(rom: Rom) {
        this.rom = rom

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
        } catch (_: Exception) {
            executor.shutdown()
        }
    }

    private fun readNextInstruction(): Instruction {
        val p = PManager.p.read()
        val address1 = byteArrayToInt(p)
        val address2 = byteArrayToInt(p+1)
        val byte1 = rom!!.read(address1)
        val byte2 = rom!!.read(address2)
        return instructionFactory.createInstruction(byteArrayOf(byte1, byte2))
    }
}