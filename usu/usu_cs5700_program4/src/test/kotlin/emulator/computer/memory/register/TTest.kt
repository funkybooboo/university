package emulator.computer.memory.register

import com.natestott.emulator.computer.memory.register.T
import org.junit.jupiter.api.BeforeEach
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class TTest {

    private lateinit var t: T

    @BeforeEach
    fun setUp() {
        t = T()
    }

    @Test
    fun testRead() {
        val initialValue = byteArrayOf(0x0A)
        t.write(initialValue)

        val readBytes = t.read()

        assertEquals(initialValue[0], readBytes[0])
    }

    @Test
    fun testWrite() {
        val validByte = byteArrayOf(0x0A)
        t.write(validByte)
        assertEquals(0x0A, t.read()[0].toInt() )
    }

    @Test
    fun testWrite_invalidSize() {
        val invalidBytes = byteArrayOf(0x01, 0x02)

        val exception = assertFailsWith<IllegalArgumentException> {
            t.write(invalidBytes)
        }

        assertEquals("ByteArray must be of size 1.", exception.message)
    }
}
