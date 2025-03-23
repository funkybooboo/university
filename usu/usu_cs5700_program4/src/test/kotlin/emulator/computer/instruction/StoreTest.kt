package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.Store
import com.natestott.emulator.computer.memory.register.RManager.r
import kotlin.test.Test
import kotlin.test.assertEquals

class StoreTest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x00, 0x23, 0x45)
        val instruction = Store(nibbles)

        instruction.processNibbles()

        assertEquals(r[0], instruction.rx)
        assertEquals(53, instruction.byte)
    }


    @Test
    fun testPerformOperation() {
        val nibbles = byteArrayOf(0x00, 0x23, 0x45)
        val instruction = Store(nibbles)

        instruction.processNibbles()

        instruction.performOperation()

        assertEquals(53, r[0].read()[0])
    }

}