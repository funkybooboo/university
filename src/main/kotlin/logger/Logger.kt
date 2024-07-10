package logger

import java.lang.Exception
import java.text.SimpleDateFormat
import java.util.*

abstract class Logger {

    private val dateFormat = SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

    fun formatLogMessage(level: Level, threadId: String, message: String, exception: Exception?=null): String {
        val formattedTime = dateFormat.format(Date())
        val exceptionMessage = exception?.let { "- Exception: ${it.message}" } ?: ""
        return "[$threadId] [${level.name}] $formattedTime - $message $exceptionMessage"
    }

    abstract fun log(level: Level, threadId: String,  message: String, exception: Exception?=null)
}