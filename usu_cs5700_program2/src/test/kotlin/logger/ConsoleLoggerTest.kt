package logger

import junit.framework.TestCase.assertTrue
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import kotlin.test.Test
import kotlin.test.assertNotNull

class ConsoleLoggerTest {
    @Test
    fun testConstruction() {
        val consoleLogger = ConsoleLogger()
        assertNotNull(consoleLogger)
    }

    @Test
    fun testFormattedMessage() {
        val consoleLogger = ConsoleLogger()

        val threadId = "1"
        val message = "Test log message"

        val formattedMessage = consoleLogger.formatLogMessage(Logger.Level.INFO, threadId, message)
        assertNotNull(formattedMessage)
    }

    @Test
    fun testLog() {
        // Redirect System.out for testing
        val outputStreamCaptor = ByteArrayOutputStream()
        System.setOut(PrintStream(outputStreamCaptor))

        val consoleLogger = ConsoleLogger()

        val threadId = "1"
        val message = "Test log message"

        consoleLogger.log(Logger.Level.INFO, threadId, message)

        // Verify that the log message was printed to System.out
        val consoleOutput = outputStreamCaptor.toString().trim()
        assertTrue(consoleOutput.contains("[1] [INFO]"))
        assertTrue(consoleOutput.contains(message))

        // Reset System.out
        System.setOut(System.out)
    }
}