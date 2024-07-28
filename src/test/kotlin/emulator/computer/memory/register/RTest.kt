package emulator.computer.memory.register

import com.natestott.emulator.computer.memory.register.R
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.BeforeEach
import kotlin.test.Test
import kotlin.test.assertEquals

class RTest {

    private lateinit var r: R

    @BeforeEach
    fun setUp() {
        r = R()
    }

    @Test
    fun testRead() {
        val initialValue = byteArrayOf(0x0A)
        r.write(initialValue)

        val readBytes = r.read()

        assertEquals(initialValue[0], readBytes[0])
    }

    @Test
    fun testWrite() {
        val validByte = byteArrayOf(0x0A)
        r.write(validByte)
        assertEquals(0x0A, r.read()[0].toInt())
    }

    @Test
    fun testWrite_invalidSize() {
        val invalidBytes = byteArrayOf(0x01, 0x02) // Example invalid bytes

        val exception = assertThrows(IllegalArgumentException::class.java) {
            r.write(invalidBytes)
        }

        assert(exception.message?.contains("must be of size 1") ?: false)
    }
}
