package logger

import java.io.FileWriter
import java.io.PrintWriter

class FileLogger(private val filePath: String) : Logger() {

    override fun log(level: Level, threadId: String, message: String, exception: Exception?) {
        writeToFile(formatLogMessage(level, threadId, message, exception))
    }

    private fun writeToFile(logMessage: String) {
        try {
            PrintWriter(FileWriter(filePath, true)).use { writer ->
                writer.println(logMessage)
            }
        } catch (e: Exception) {
            println("Error writing to file: ${e.message}")
        }
    }
}