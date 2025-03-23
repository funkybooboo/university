package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.ConvertByteToAscii
import com.natestott.emulator.computer.memory.register.RManager
import kotlin.test.Test
import kotlin.test.assertEquals

class ConvertByteToAsciiTest {

    @Test
    fun testProcessNibbles() {
        val rx = RManager.r[0]
        val ry = RManager.r[1]

        rx.write(byteArrayOf(0x00))
        ry.write(byteArrayOf(0x00))

        val nibbles = byteArrayOf(0x00, 0x01, 0x00)
        val instruction = ConvertByteToAscii(nibbles)

        instruction.processNibbles()

        assertEquals(rx, instruction.rx)
        assertEquals(ry, instruction.ry)
    }

    @Test
    fun testPerformOperation() {
        val rx = RManager.r[0]
        rx.write(byteArrayOf(0x05))

        val ry = RManager.r[1]
        ry.write(byteArrayOf(0x00))

        val nibbles = byteArrayOf(0x00, 0x01, 0x00)
        val instruction = ConvertByteToAscii(nibbles)

        instruction.processNibbles()

        instruction.performOperation()

        assertEquals('5'.code.toByte(), ry.read()[0])
    }
}
