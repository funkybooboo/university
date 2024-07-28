package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.Sub
import com.natestott.emulator.computer.memory.register.RManager.r
import kotlin.test.Test
import kotlin.test.assertEquals

class SubTest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x00, 0x01, 0x03)
        val instruction = Sub(nibbles)

        instruction.processNibbles()

        assertEquals(r[0], instruction.rx)
        assertEquals(r[1], instruction.ry)
        assertEquals(r[3], instruction.rz)
    }


    @Test
    fun testPerformOperation() {
        val nibbles = byteArrayOf(0x00, 0x01, 0x03)
        val instruction = Sub(nibbles)

        r[0].write(byteArrayOf(0x10))
        r[1].write(byteArrayOf(0x05))

        instruction.processNibbles()
        instruction.performOperation()

        assertEquals(16, r[0].read()[0])
    }

}