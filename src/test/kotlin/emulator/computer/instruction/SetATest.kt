package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.SetA
import com.natestott.emulator.computer.memory.register.AManager.a
import kotlin.test.Test
import kotlin.test.assertEquals

class SetATest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x12, 0x34, 0x56)
        val instruction = SetA(nibbles)

        instruction.processNibbles()

        assertEquals(0x13, instruction.addressBytes[0])
        assertEquals(0x56, instruction.addressBytes[1])
    }

    @Test
    fun testPerformOperation() {
        val nibbles = byteArrayOf(0x12, 0x34, 0x56)
        val instruction = SetA(nibbles)

        instruction.processNibbles()

        instruction.performOperation()

        assertEquals(0x13, a.read()[0])
        assertEquals(0x56, a.read()[1])
    }
}
