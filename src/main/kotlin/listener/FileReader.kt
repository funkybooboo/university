package listener
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import java.io.File

class FileReader(
    private val queue: Queue<String>,
    private val fileName: String,
): UpdateListener {
    override suspend fun listen() = withContext(Dispatchers.IO) {
        val file = File(fileName)
        val lines = file.readLines()
        lines.forEach { line ->
            queue.enqueue(line)
        }
    }
}