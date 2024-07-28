package emulator.computer.memory.contiguous

import com.natestott.emulator.computer.memory.contiguous.Rom
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.BeforeEach
import kotlin.test.Test
import kotlin.test.assertEquals

class RomTest {

    private lateinit var rom: Rom

    @BeforeEach
    fun setUp() {
        val bytes = ByteArray(4096)
        rom = Rom(bytes)
    }

    @Test
    fun testRead() {
        val address = 0x100
        val expectedByte: Byte = 0x42
        rom.bytes[address] = expectedByte

        val result = rom.read(address)

        assertEquals(expectedByte, result)
    }

    @Test
    fun testWrite() {
        val address = 0x200
        val byteToWrite: Byte = 0xB

        val exception = assertThrows(UnsupportedOperationException::class.java) {
            rom.write(address, byteToWrite)
        }

        assertEquals("Cannot write to ROM", exception.message)
    }

}
