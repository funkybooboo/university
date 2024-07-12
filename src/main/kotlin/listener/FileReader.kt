package listener

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import java.io.File
import logger.Logger.Level
import manager.LoggerManager.logger

class FileReader(
    queue: Queue<String>,
    private val filePath: String,
): UpdateListener(queue) {
    override suspend fun listen() = withContext(Dispatchers.IO) {
        try {
            val file = File(filePath)
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Starting to read file: $filePath")
            val lines = file.readLines()
            lines.forEach { line ->
                queue.enqueue(line)
                logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Enqueued line: $line")
            }
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "File reading completed: $filePath")
        } catch (e: Exception) {
            logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Error reading file: $filePath - ${e.message}")
            e.printStackTrace()
        }
    }
}
