package emulator.computer.instruction

import com.natestott.emulator.computer.Screen
import com.natestott.emulator.computer.ScreenManager
import com.natestott.emulator.computer.instruction.Draw
import com.natestott.emulator.computer.memory.register.RManager
import kotlin.test.Test
import kotlin.test.assertEquals

class DrawTest {
    @Test
    fun testProcessNibbles() {
        val nibbles = byteArrayOf(0x00, 0x01, 0x02)
        val instruction = Draw(nibbles)

        instruction.processNibbles()

        assertEquals(RManager.r[0], instruction.rx)
        assertEquals(0x01.toByte(), instruction.row)
        assertEquals(0x02.toByte(), instruction.col)
    }

    @Test
    fun testPerformOperation_ValidASCII() {
        val screen = ScreenManager.screen
        val rx = RManager.r[0]
        rx.write(byteArrayOf(0x41))  // ASCII 'A'
        val row = 2.toByte()
        val col = 3.toByte()

        val instruction = Draw(byteArrayOf(0x00, row, col))
        instruction.rx = rx
        instruction.row = row
        instruction.col = col

        instruction.performOperation()

        val buffer = screen.buffer
        val address = row.toInt() * Screen.BUFFER_WIDTH + col.toInt()
        assertEquals(0x41.toByte(), buffer[address])
    }
}