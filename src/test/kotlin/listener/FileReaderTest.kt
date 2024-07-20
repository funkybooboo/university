package listener

import kotlinx.coroutines.channels.Channel
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
        val channel = Channel<String>()

        val fileReader = FileReader(channel, filePath)
        assertNotNull(fileReader)
    }

    @Test
    fun testInvalidConstruction() {
        val channel = Channel<String>()

        assertThrows(IllegalArgumentException::class.java) {
            FileReader(channel, "")
        }
    }

    @Test
    fun testListen() {
        val testFilePath = "data/test.txt"
        val channel = Channel<String>()
        val fileReader = FileReader(channel, testFilePath)

        val testFile = File(testFilePath)
        testFile.writeText("Line 1\nLine 2\nLine 3\n")

        runBlocking {
            fileReader.listen()
            assertEquals("Line 1", channel.receive())
            assertEquals("Line 2", channel.receive())
            assertEquals("Line 3", channel.receive())
        }
    }
}