package listener
import kotlinx.coroutines.coroutineScope
import java.io.File

class FileReader(private val queue: Queue<String>, private val fileName: String): UpdateListener {
    override suspend  fun listen() {
        coroutineScope {
            val file = File(fileName)
            file.forEachLine { line ->
                queue.enqueue(line)
            }
        }
    }
}