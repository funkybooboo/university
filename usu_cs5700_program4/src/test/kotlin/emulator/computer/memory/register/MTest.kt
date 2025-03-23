package emulator.computer.memory.register

import com.natestott.emulator.computer.memory.register.M
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.BeforeEach
import kotlin.test.Test
import kotlin.test.assertEquals

class MTest {

    private lateinit var m: M

    @BeforeEach
    fun setUp() {
        // Initialize the M register
        m = M()
    }

    @Test
    fun testRead() {
        val initialValue = byteArrayOf(0x01)
        m.write(initialValue)

        val readBytes = m.read()

        assertEquals(initialValue[0], readBytes[0])
    }

    @Test
    fun testWrite() {
        val validByte = byteArrayOf(0x01)
        m.write(validByte)
        assertEquals(0x01, m.read()[0].toInt() and 0xFF)
    }

    @Test
    fun testWrite_invalidSize() {
        val invalidBytes = byteArrayOf(0x01, 0x02)

        val exception = assertThrows(IllegalArgumentException::class.java) {
            m.write(invalidBytes)
        }

        assert(exception.message?.contains("must be of size 1") ?: false)
    }

    @Test
    fun testWrite_invalidValue() {
        val invalidByte = byteArrayOf(0x02)

        val exception = assertThrows(IllegalArgumentException::class.java) {
            m.write(invalidByte)
        }

        assert(exception.message?.contains("Invalid flag value. Must be 0 or 1.") ?: false)
    }
}
