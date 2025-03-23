package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.SwitchMemory
import com.natestott.emulator.computer.memory.register.MManager.m
import org.junit.jupiter.api.assertDoesNotThrow
import kotlin.test.Test
import kotlin.test.assertEquals

class SwitchMemoryTest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x00, 0x00, 0x00)
        val instruction = SwitchMemory(nibbles)

        assertDoesNotThrow { instruction.processNibbles() }
    }

    @Test
    fun testPerformOperation() {
        m.write(byteArrayOf(0x00))

        val instruction = SwitchMemory(byteArrayOf(0x00, 0x00, 0x00))

        instruction.performOperation()

        assertEquals(1, m.read()[0])
    }

}