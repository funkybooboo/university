package logger

import java.util.Date

class Console : Logger {
    override fun log(level: Level, message: String, exception: Exception?) {
        val time = Date()
        val formattedTime = time.toString() 
        
        val formattedMessage = buildString {
            append("[${level.toString().uppercase()}] $formattedTime - ")
            append(message ?: "")
            exception?.let {
                append(" Exception: ${it.message}")
            }
        }
        
        println(formattedMessage)
    }
}
