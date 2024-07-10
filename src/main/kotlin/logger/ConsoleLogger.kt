package logger

class ConsoleLogger : Logger() {
    override fun log(level: Level, threadId: String, message: String, exception: Exception?) {
        println(formatLogMessage(level, threadId, message, exception))
    }
}
