package listener
import java.io.File

class FileReader(private val queue: Queue<String>, private val fileName: String): UpdateListener {
    override fun listen() {
        val file = File(fileName)
        file.forEachLine { line ->
            queue.enqueue(line)
        }
    }
}