package manager

import logger.CompositeLogger
import logger.ConsoleLogger
import logger.FileLogger

object LoggerManager {
    val logger = CompositeLogger()

    init {
        val fileLogger = FileLogger("log/logs.log")
        val consoleLogger = ConsoleLogger()
        logger.registerLogger(consoleLogger)
        logger.registerLogger(fileLogger)
    }
}