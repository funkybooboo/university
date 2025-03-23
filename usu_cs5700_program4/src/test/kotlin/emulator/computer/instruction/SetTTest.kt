package emulator.computer.instruction

import com.natestott.emulator.computer.combineNibblesToByte
import com.natestott.emulator.computer.instruction.SetT
import com.natestott.emulator.computer.memory.register.TManager
import kotlin.test.Test
import kotlin.test.assertEquals

class SetTTest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x1, 0x1, 0x0)
        val instruction = SetT(nibbles)
        instruction.processNibbles()
        assertEquals(instruction.value, combineNibblesToByte(nibbles[0], nibbles[1]))
    }

    @Test
    fun testPerformOperation() {
        val nibbles = byteArrayOf(0x1, 0x1, 0x0)
        val instruction = SetT(nibbles)
        instruction.processNibbles()
        instruction.performOperation()
        assertEquals(TManager.t.read()[0], combineNibblesToByte(nibbles[0], nibbles[1]))
    }
}
