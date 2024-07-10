package logger

import java.lang.Exception

interface Logger {
    fun log(level: Level, message: String, exception: Exception?=null)
}