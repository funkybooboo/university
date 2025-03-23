package com.natestott.emulator.logger

class ConsoleLogger : Logger() {
    override fun log(level: Level, message: String, exception: Exception?) {
        println(formatLogMessage(level, message, exception))
    }
}
