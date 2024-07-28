package emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.instruction.SkipNotEqual
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.PManager.p
import com.natestott.emulator.computer.memory.register.RManager.r
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class SkipNotEqualTest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x00, 0x01, 0x00)
        val instruction = SkipNotEqual(nibbles)

        instruction.processNibbles()

        assertEquals(r[0], instruction.rx)
        assertEquals(r[1], instruction.ry)
    }


    @Test
    fun testPerformOperation() {
        val nibbles = byteArrayOf(0x00, 0x01, 0x00)
        val instruction = SkipNotEqual(nibbles)

        instruction.processNibbles()

        r[0].write(byteArrayOf(0x42))
        r[1].write(byteArrayOf(0x42))

        instruction.performOperation()

        assertFalse(instruction.shouldSkip)

        r[0].write(byteArrayOf(0x42))
        r[1].write(byteArrayOf(0x43))

        instruction.performOperation()

        assertTrue(instruction.shouldSkip)
    }

    @Test
    fun testIncrementProgramCounter() {
        val nibbles = byteArrayOf(0x01, 0x02, 0x00)
        val instruction = SkipNotEqual(nibbles)

        instruction.processNibbles()
        instruction.shouldSkip = true

        val initialPC = 0x0100
        p.write(intToByteArray(initialPC))

        instruction.incrementProgramCounter()

        val expectedNewPC = initialPC + 4  // Skip case
        assertEquals(expectedNewPC, byteArrayToInt(p.read()))

        instruction.shouldSkip = false

        instruction.incrementProgramCounter()

        val expectedNonSkipPC = expectedNewPC + 2  // Non-skip case
        assertEquals(expectedNonSkipPC, byteArrayToInt(p.read()))
    }
}