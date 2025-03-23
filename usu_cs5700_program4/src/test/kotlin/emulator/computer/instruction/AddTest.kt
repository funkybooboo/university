package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.Add
import com.natestott.emulator.computer.memory.register.RManager
import org.junit.jupiter.api.Assertions.assertEquals
import kotlin.test.Test

class AddTest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x00, 0x01, 0x02)
        val instruction = Add(nibbles)

        instruction.processNibbles()

        assertEquals(RManager.r[0], instruction.rx)
        assertEquals(RManager.r[1], instruction.ry)
        assertEquals(RManager.r[2], instruction.rz)
    }

    @Test
    fun testPerformOperation() {
        val rx = RManager.r[0]
        rx.write(byteArrayOf(0x05))
        val ry = RManager.r[1]
        ry.write(byteArrayOf(0x03))
        val rz = RManager.r[2]
        rz.write(byteArrayOf(0x00))

        val nibbles = byteArrayOf(0x00, 0x01, 0x02)
        val instruction = Add(nibbles)
        instruction.processNibbles()

        instruction.performOperation()

        assertEquals(0x08.toByte(), rz.read()[0])
    }
}