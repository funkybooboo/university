package logger

import junit.framework.TestCase.*
import logger.Logger.Level
import kotlin.test.Test
import kotlin.test.assertNotNull

class CompositeLoggerTest {
    @Test
    fun testConstruction() {
        val compositeLogger = CompositeLogger()
        assertNotNull(compositeLogger)
    }

    @Test
    fun testFormattedMessage() {
        val logger = CompositeLogger()

        val threadId = "1"
        val message = "Test log message"

        val formattedMessage = logger.formatLogMessage(Level.INFO, threadId, message)

        assertNotNull(formattedMessage)
    }

    @Test
    fun testRegisterLogger() {
        val compositeLogger = CompositeLogger()
        val logger = ConsoleLogger()

        compositeLogger.registerLogger(logger)

        assertTrue(compositeLogger.loggers.contains(logger))
    }

    @Test
    fun testUnregisterLogger() {
        val compositeLogger = CompositeLogger()
        val logger = ConsoleLogger()

        compositeLogger.registerLogger(logger)
        assertTrue(compositeLogger.loggers.contains(logger))

        compositeLogger.unregisterLogger(logger)
        assertFalse(compositeLogger.loggers.contains(logger))
    }

    @Test
    fun testLog() {
        val compositeLogger = CompositeLogger()
        val logger1 = ConsoleLogger()
        val logger2 = ConsoleLogger()

        compositeLogger.registerLogger(logger1)
        compositeLogger.registerLogger(logger2)

        val threadId = "1"
        val message = "Test log message"

        compositeLogger.log(Level.INFO, threadId, message)
    }
}