package com.natestott.emulator.logger

class CompositeLogger() : Logger() {
    private val loggers: MutableList<Logger> = mutableListOf()

    fun registerLogger(logger: Logger) {
        loggers.add(logger)
    }

    fun unregisterLogger(logger: Logger) {
        loggers.remove(logger)
    }

    override fun log(level: Level, message: String, exception: Exception?) {
        loggers.forEach { logger ->
            logger.log(level, message, exception)
        }
    }
}