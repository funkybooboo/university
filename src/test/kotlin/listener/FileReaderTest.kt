package listener

import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import java.io.File
import kotlin.test.Test
import kotlin.test.assertNotNull

class FileReaderTest {
    @Test
    fun testConstruction() {
        val filePath = "data/test.txt"
        val queue = Queue<String>()

        val fileReader = FileReader(queue, filePath)
        assertNotNull(fileReader)
    }

    @Test
    fun testInvalidConstruction() {
        val queue = Queue<String>()

        assertThrows(IllegalArgumentException::class.java) {
            FileReader(queue, "")
        }
    }

    @Test
    fun testListen() {
        val testFilePath = "data/test.txt"
        val queue = Queue<String>()
        val fileReader = FileReader(queue, testFilePath)

        val testFile = File(testFilePath)
        testFile.writeText("Line 1\nLine 2\nLine 3\n")

        runBlocking {
            fileReader.listen()
        }

        assertEquals("Line 1", queue.dequeue())
        assertEquals("Line 2", queue.dequeue())
        assertEquals("Line 3", queue.dequeue())
    }
}