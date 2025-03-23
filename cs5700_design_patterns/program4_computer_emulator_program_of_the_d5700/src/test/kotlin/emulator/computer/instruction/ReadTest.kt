package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.Read
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.contiguous.RamManager
import com.natestott.emulator.computer.memory.contiguous.RomManager
import com.natestott.emulator.computer.memory.register.AManager
import com.natestott.emulator.computer.memory.register.MManager
import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager
import org.junit.jupiter.api.Assertions.assertThrows
import kotlin.test.Test
import kotlin.test.assertEquals

class ReadTest {
    @Test
    fun testProcessNibbles() {
        val rxIndex = 0x2
        val nibbles = byteArrayOf(rxIndex.toByte(), 0x00, 0x00)

        val instruction = Read(nibbles)

        instruction.processNibbles()

        assertEquals(RManager.r[rxIndex], instruction.rx)
    }

    @Test
    fun testPerformOperation_ReadFromRAM() {
        val ram = RamManager.ram
        val address = 0x100
        ram.write(address, 0x42.toByte())

        val rx = R()
        val nibbles = byteArrayOf(0x02, 0x01, 0x00)
        val instruction = Read(nibbles)
        instruction.rx = rx

        MManager.m.write(byteArrayOf(0x00))

        AManager.a.write(intToByteArray(address))

        instruction.performOperation()

        assertEquals(0x42.toByte(), rx.read()[0])
    }

    @Test
    fun testPerformOperation_ReadFromROM() {
        val address = 0x200
        val memory = ByteArray(4096)
        memory[address] = 0x84.toByte()
        RomManager.initializeRom(memory)

        val rx = R()
        val nibbles = byteArrayOf(0x02, 0x02, 0x00)
        val instruction = Read(nibbles)
        instruction.rx = rx

        MManager.m.write(byteArrayOf(0x01))

        AManager.a.write(intToByteArray(address))

        instruction.performOperation()

        assertEquals(0x84.toByte(), rx.read()[0])
    }

    @Test
    fun testPerformOperation_InvalidMemoryType() {
        val nibbles = byteArrayOf(0x02, 0x01, 0x00)
        val instruction = Read(nibbles)

        assertThrows(IllegalArgumentException::class.java) {
            MManager.m.write(byteArrayOf(0x02))
            instruction.performOperation()
        }
    }
}