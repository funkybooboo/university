package logger

import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import java.io.File
import java.nio.file.Files
import kotlin.test.Test
import kotlin.test.assertNotNull

class FileLoggerTest {
    @Test
    fun testConstruction() {
        val filePath = "logs/test.log"
        val fileLogger = FileLogger(filePath)
        assertNotNull(fileLogger)
    }

    @Test
    fun testInvalidConstruction() {
        assertThrows(IllegalArgumentException::class.java) {
            FileLogger("")
        }
    }

    @Test
    fun testFormattedMessage() {
        val filePath = "logs/test.log"
        val fileLogger = FileLogger(filePath)

        val threadId = "1"
        val message = "Test log message"

        val formattedMessage = fileLogger.formatLogMessage(Logger.Level.INFO, threadId, message)

        assertNotNull(formattedMessage)
    }

    @Test
    fun testLog() {
        val filePath = "data/test.txt"
        val fileLogger = FileLogger(filePath)

        val threadId = "1"
        val message = "Test log message"

        fileLogger.log(Logger.Level.INFO, threadId, message)

        // Read the content of the file and check if the log message is present
        val logFile = File(filePath)
        val lines = Files.readAllLines(logFile.toPath())
        val lastLine = lines.lastOrNull()

        assertTrue(lastLine != null && lastLine.contains(message))
    }

    @Test
    fun testWriteToFile() {
        val filePath = "data/test.txt"
        val fileLogger = FileLogger(filePath)

        val message = "Test log message"

        fileLogger.writeToFile(message)

        // Read the content of the file and check if the message is present
        val logFile = File(filePath)
        val lines = Files.readAllLines(logFile.toPath())
        val lastLine = lines.lastOrNull()

        assertTrue(lastLine != null && lastLine.contains(message))
    }
}