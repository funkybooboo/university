package com.natestott.emulator

import com.natestott.emulator.computer.ControlUnit
import com.natestott.emulator.computer.Cpu
import com.natestott.emulator.computer.memory.contiguous.Rom
import java.io.File
import java.io.IOException
import com.natestott.emulator.computer.ChannelManager.channel

class ComputerEmulator {
    fun start() {
        while (true) {
            val pathToBinaryFile = getPathToBinaryFile()
            if (pathToBinaryFile.lowercase() == "q" || pathToBinaryFile.lowercase() == "quit") break
            val binaryFile = getBinaryFile(pathToBinaryFile)
            val binaryProgram = getBinaryProgramFromBinaryFile(binaryFile)
            val rom = getRomFromBinaryProgram(binaryProgram)

            val controlUnit = ControlUnit(rom, channel)
            val cpu = Cpu(channel)
            // TODO thread stuff with controlunit and cpu
        }
    }

    private fun getPathToBinaryFile(): String {
        println("Path to binary file or type q to quit: ")
        val pathToBinaryFile = readlnOrNull() ?: throw IOException("Please provide a path to a binary file")
        return pathToBinaryFile
    }

    private fun getBinaryFile(pathToBinaryFile: String): File {
        val binaryFile = File(pathToBinaryFile)
        return binaryFile
    }

    private fun getBinaryProgramFromBinaryFile(binaryFile: File): ByteArray {
        try {
            val binaryProgram = binaryFile.readBytes()
            return binaryProgram
        } catch (e: IOException) {
            throw IOException("Failed to read binary file", e)
        }
    }

    private fun getRomFromBinaryProgram(binaryProgram: ByteArray): Rom {
        if (binaryProgram.size > 4096) {
            throw IllegalArgumentException("binary program cannot be more then 4096 bytes")
        }
        val memory = ByteArray(4096)
        for (i in 0..binaryProgram.size) {
            memory[i] = binaryProgram[i]
        }
        val rom = Rom(memory)
        return rom
    }
}