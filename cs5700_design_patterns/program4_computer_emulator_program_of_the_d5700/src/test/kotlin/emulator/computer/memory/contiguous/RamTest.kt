package emulator.computer.memory.contiguous

import com.natestott.emulator.computer.memory.contiguous.Ram
import kotlin.test.Test
import kotlin.test.assertEquals

class RamTest {
    @Test
    fun testRead() {
        val ram = Ram()

        val address = 0x100
        val expectedByte: Byte = 0x42
        ram.bytes[address] = expectedByte

        val result = ram.read(address)

        assertEquals(expectedByte, result)
    }

    @Test
    fun testWrite() {
        val ram = Ram()

        val address = 0x200
        val byteToWrite: Byte = 0xB

        ram.write(address, byteToWrite)

        val result = ram.bytes[address]

        assertEquals(byteToWrite, result)
    }
}