package emulator.computer.memory.register

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.memory.register.P
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.BeforeEach
import kotlin.test.Test
import kotlin.test.assertEquals

class PTest {

    private lateinit var p: P

    @BeforeEach
    fun setUp() {
        p = P()
    }

    @Test
    fun testRead() {
        val initialValue = byteArrayOf(0x00, 0x02)
        p.write(initialValue)

        val readBytes = p.read()

        assertEquals(initialValue[0], readBytes[0])
        assertEquals(initialValue[1], readBytes[1])
    }


    @Test
    fun testWrite() {
        val validBytes = byteArrayOf(0x00, 0x02)
        p.write(validBytes)
        assertEquals(0x0002, byteArrayToInt(p.read()))
    }

    @Test
    fun testWrite_invalidSize() {
        val invalidBytes = byteArrayOf(0x01)

        val exception = assertThrows(IllegalArgumentException::class.java) {
            p.write(invalidBytes)
        }

        assert(exception.message?.contains("must be of size 2") ?: false)
    }

    @Test
    fun testWrite_invalidValue() {
        val invalidBytes = byteArrayOf(0x00, 0x03)

        val exception = assertThrows(IllegalArgumentException::class.java) {
            p.write(invalidBytes)
        }

        assert(exception.message?.contains("Integer value must be divisible by 2.") ?: false)
    }
}
