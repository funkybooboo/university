package manager

import listener.FileReader
import manager.QueueManager.queue

object FileReaderManager {
    private const val listenerFilePath = "data/test.txt"
    val fileReader = FileReader(queue, listenerFilePath)
}