package com.natestott.emulator

import com.natestott.emulator.computer.Cpu
import com.natestott.emulator.computer.memory.contiguous.RomManager
import java.io.File
import java.io.IOException
import com.natestott.emulator.computer.memory.contiguous.Rom
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class ComputerEmulator {

    private val cpu = Cpu()

    fun run() {
        logger.log(Level.INFO, "Starting the computer emulator")

        try {
            val pathToBinaryFile = getPathToBinaryFile()
            logger.log(Level.INFO, "Path to binary file obtained: $pathToBinaryFile")

            val binaryFile = getBinaryFile(pathToBinaryFile)
            logger.log(Level.INFO, "Binary file object created: ${binaryFile.absolutePath}")

            val binaryProgram = getBinaryProgramFromBinaryFile(binaryFile)
            logger.log(Level.INFO, "Binary file read successfully. Size: ${binaryProgram.size} bytes")

            val rom = getRomFromBinaryProgram(binaryProgram)
            logger.log(Level.INFO, "ROM initialized successfully")

            cpu.executeProgram(rom)
            logger.log(Level.INFO, "Program executed successfully")

        } catch (e: IOException) {
            logger.log(Level.ERROR, "An I/O error occurred: ${e.message}", e)
        } catch (e: Exception) {
            logger.log(Level.ERROR, "An unexpected error occurred: ${e.message}", e)
        }
    }

    private fun getPathToBinaryFile(): String {
        println("Path to binary file: ")
        val pathToBinaryFile = readlnOrNull() ?: throw IOException("No path provided")
        if (pathToBinaryFile.lowercase() == "q") {
            logger.log(Level.INFO, "User chose to quit")
            throw IOException("User opted to quit")
        }
        return pathToBinaryFile
    }

    private fun getBinaryFile(pathToBinaryFile: String): File {
        val file = File(pathToBinaryFile)
        if (!file.exists()) {
            logger.log(Level.ERROR, "The file does not exist: $pathToBinaryFile")
            throw IOException("File not found: $pathToBinaryFile")
        }
        return file
    }

    private fun getBinaryProgramFromBinaryFile(binaryFile: File): ByteArray {
        return try {
            binaryFile.readBytes()
        } catch (e: IOException) {
            logger.log(Level.ERROR, "Failed to read binary file: ${binaryFile.absolutePath}", e)
            throw IOException("Failed to read binary file", e)
        }
    }

    private fun getRomFromBinaryProgram(binaryProgram: ByteArray): Rom {
        val memory = ByteArray(4096)
        for (i in binaryProgram.indices) {
            memory[i] = binaryProgram[i]
        }
        RomManager.initializeRom(memory)
        val rom = RomManager.getRom()
        if (rom == null) {
            logger.log(Level.ERROR, "Failed to initialize ROM")
            throw IOException("Failed to initialize ROM")
        }
        return rom
    }
}
