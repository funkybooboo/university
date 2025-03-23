package com.natestott.emulator.logger

import java.io.FileWriter
import java.io.PrintWriter

class FileLogger(private val filePath: String) : Logger() {

    init {
        if (filePath.isBlank()) {
            throw IllegalArgumentException("blank file path")
        }
    }

    override fun log(level: Level, message: String, exception: Exception?) {
        writeToFile(formatLogMessage(level, message, exception))
    }

    fun writeToFile(message: String) {
        try {
            PrintWriter(FileWriter(filePath, true)).use { writer ->
                writer.println(message)
            }
        } catch (e: Exception) {
            println("Error writing to file: ${e.message}")
        }
    }
}