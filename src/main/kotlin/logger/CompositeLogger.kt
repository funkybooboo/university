package logger

class CompositeLogger : Logger() {
    private val loggers: MutableList<Logger> = mutableListOf()

    fun registerLogger(logger: Logger) {
        loggers.add(logger)
    }

    fun unregisterLogger(logger: Logger) {
        loggers.remove(logger)
    }

    override fun log(level: Level, threadId: String, message: String, exception: Exception?) {
        val formattedMessage = formatLogMessage(level, threadId, message, exception)
        loggers.forEach { logger ->
            logger.log(level, threadId, formattedMessage, exception)
        }
    }
}