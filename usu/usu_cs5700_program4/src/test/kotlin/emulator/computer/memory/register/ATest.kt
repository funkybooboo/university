package emulator.computer.memory.register

import com.natestott.emulator.computer.memory.register.A
import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.BeforeEach
import kotlin.test.Test

class ATest {

    private lateinit var a: A

    @BeforeEach
    fun setUp() {
        // Initialize the A register
        a = A()
    }

    @Test
    fun testRead() {
        val bytesToWrite = byteArrayOf(0x12, 0x34)
        a.write(bytesToWrite)

        val readBytes = a.read()

        assertArrayEquals(bytesToWrite, readBytes)
    }


    @Test
    fun testWrite() {
        val bytesToWrite = byteArrayOf(0xB, 0xD)

        a.write(bytesToWrite)

        val readBytes = a.read()
        assertArrayEquals(bytesToWrite, readBytes)
    }

    @Test
    fun testWrite_invalidSize() {
        val invalidBytes = byteArrayOf(0x01, 0x02, 0x03)

        val exception = assertThrows(IllegalArgumentException::class.java) {
            a.write(invalidBytes)
        }

        assert(exception.message?.contains("must be of size 2") ?: false)
    }
}
