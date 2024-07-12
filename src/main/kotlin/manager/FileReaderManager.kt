package manager

import listener.FileReader

object FileReaderManager {
    private const val listenerFilePath = "data/test.txt"
    val fileReader = FileReader(queue, listenerFilePath)
}