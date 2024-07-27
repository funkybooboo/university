package com.natestott.emulator

import com.natestott.emulator.computer.Cpu
import com.natestott.emulator.computer.memory.contiguous.RomManager
import java.io.File
import java.io.IOException
import com.natestott.emulator.computer.memory.contiguous.Rom

class ComputerEmulator {

    private val cpu = Cpu()

    fun start() {
        while (true) {
            val pathToBinaryFile = getPathToBinaryFile()
            if (pathToBinaryFile.lowercase() == "q" || pathToBinaryFile.lowercase() == "quit") break
            val binaryFile = getBinaryFile(pathToBinaryFile)
            val binaryProgram = getBinaryProgramFromBinaryFile(binaryFile)
            val rom = getRomFromBinaryProgram(binaryProgram)
            cpu.executeProgram(rom)
        }
    }

    private fun getPathToBinaryFile(): String {
        println("Path to binary file or type q to quit: ")
        val pathToBinaryFile = readlnOrNull() ?: throw IOException("Please provide a path to a binary file")
        return pathToBinaryFile
    }

    private fun getBinaryFile(pathToBinaryFile: String): File {
        return File(pathToBinaryFile)
    }

    private fun getBinaryProgramFromBinaryFile(binaryFile: File): ByteArray {
        return try {
            binaryFile.readBytes()
        } catch (e: IOException) {
            throw IOException("Failed to read binary file", e)
        }
    }

    private fun getRomFromBinaryProgram(binaryProgram: ByteArray): Rom {
        if (binaryProgram.size < 4096) {
            throw IllegalArgumentException("Binary program cannot be less than 4096 bytes")
        }
        val memory = ByteArray(4096)
        for (i in binaryProgram.indices) { // Fixed to avoid IndexOutOfBoundsException
            memory[i] = binaryProgram[i]
        }
        RomManager.initializeRom(memory)
        return RomManager.getRom()!!
    }
}
