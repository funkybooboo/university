package logger

import java.io.FileWriter
import java.io.PrintWriter

class FileLogger(private val filePath: String) : Logger() {

    override fun log(level: Level, threadId: String, message: String, exception: Exception?) {
        writeToFile(formatLogMessage(level, threadId, message, exception))
    }

    private fun writeToFile(message: String) {
        try {
            PrintWriter(FileWriter(filePath, true)).use { writer ->
                writer.println(message)
            }
        } catch (e: Exception) {
            println("Error writing to file: ${e.message}")
        }
    }
}