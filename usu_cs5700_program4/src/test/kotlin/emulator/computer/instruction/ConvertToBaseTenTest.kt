package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.ConvertToBaseTen
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.contiguous.RamManager
import com.natestott.emulator.computer.memory.contiguous.RomManager
import com.natestott.emulator.computer.memory.register.AManager
import com.natestott.emulator.computer.memory.register.MManager
import com.natestott.emulator.computer.memory.register.RManager
import org.junit.jupiter.api.assertThrows
import kotlin.test.Test
import kotlin.test.assertEquals

class ConvertToBaseTenTest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x00, 0x00, 0x00)
        val instruction = ConvertToBaseTen(nibbles)

        instruction.processNibbles()

        assertEquals(RManager.r[0], instruction.rx)
    }

    @Test
    fun testPerformOperation_UsingROM() {
        RomManager.initializeRom(ByteArray(4096))
        val address = 0

        val nibbles = byteArrayOf(0x00, 0x00, 0x00)
        val instruction = ConvertToBaseTen(nibbles)
        instruction.processNibbles()

        AManager.a.write(intToByteArray(address))
        MManager.m.write(byteArrayOf(1))

        assertThrows<UnsupportedOperationException> {
            instruction.performOperation()
        }
    }

    @Test
    fun testPerformOperation_UsingRAM() {
        val ram = RamManager.ram
        val address = 0

        val rx = RManager.r[0]
        rx.write(byteArrayOf(0x11))

        val nibbles = byteArrayOf(0x00, 0x00, 0x00)
        val instruction = ConvertToBaseTen(nibbles)
        instruction.processNibbles()

        AManager.a.write(intToByteArray(address))
        MManager.m.write(byteArrayOf(0))

        instruction.performOperation()

        assertEquals(0, ram.read(address).toInt())
        assertEquals(1, ram.read(address + 1).toInt())
        assertEquals(7, ram.read(address + 2).toInt())
    }
}
