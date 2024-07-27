package com.natestott.emulator.logger

import java.lang.Exception
import java.text.SimpleDateFormat
import java.util.*

object LoggerManager {
    val logger = CompositeLogger()

    init {
        val fileLogger = FileLogger("log/logs.log")
        val consoleLogger = ConsoleLogger()
        logger.registerLogger(consoleLogger)
        logger.registerLogger(fileLogger)
    }
}

abstract class Logger() {

    enum class Level() {
        INFO,
        WARNING,
        ERROR
    }

    private val dateFormat = SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

    fun formatLogMessage(level: Level, message: String, exception: Exception?=null): String {
        val formattedTime = dateFormat.format(Date())
        val exceptionMessage = exception?.let { "- Exception: ${it.message}" } ?: ""
        return "[${level.name}] $formattedTime - $message $exceptionMessage"
    }

    abstract fun log(level: Level, message: String, exception: Exception?=null)
}