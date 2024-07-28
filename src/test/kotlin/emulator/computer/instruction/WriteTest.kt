package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.Write
import com.natestott.emulator.computer.memory.contiguous.RamManager.ram
import com.natestott.emulator.computer.memory.register.AManager.a
import com.natestott.emulator.computer.memory.register.MManager.m
import com.natestott.emulator.computer.memory.register.RManager.r
import kotlin.test.Test
import kotlin.test.assertEquals

class WriteTest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x01, 0x00, 0x00)
        val instruction = Write(nibbles)

        instruction.processNibbles()

        assertEquals(r[1], instruction.rx)
    }


    @Test
    fun testPerformOperation() {
        val initialMemoryMode = byteArrayOf(0x00)
        val initialAddress = byteArrayOf(0x00, 0x00)

        m.write(initialMemoryMode)
        a.write(initialAddress)

        val nibbles = byteArrayOf(0x01, 0x00, 0x00)
        val instruction = Write(nibbles)

        r[1].write(byteArrayOf(0x0A))

        instruction.processNibbles()
        instruction.performOperation()

        assertEquals(0x0A, ram.read(0x0000))
    }
}