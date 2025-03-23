package manager

import listener.FileReader
import manager.QueueManager.queue

object FileReaderManager {
    private const val listenerFilePath = "data/shipments0.txt"
    val fileReader = FileReader(queue, listenerFilePath)
}