package emulator.computer

import com.natestott.emulator.computer.Screen
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.BeforeEach
import kotlin.test.Test

class ScreenTest {

    private lateinit var screen: Screen

    @BeforeEach
    fun setUp() {
        screen = Screen()
    }

    @Test
    fun testDraw_ValidPosition() {
        val byte: Byte = 65 // ASCII 'A'
        val row: Byte = 2
        val col: Byte = 3

        screen.draw(byte, row, col)

        val expectedIndex = row.toInt() * Screen.BUFFER_WIDTH + col.toInt()
        assertEquals(byte, screen.buffer[expectedIndex], "Byte should be drawn to the correct position")
    }

    @Test
    fun testDraw_OutOfBounds() {
        val byte: Byte = 66 // ASCII 'B'
        val row: Byte = 10 // Out of bounds
        val col: Byte = 3

        val exception = assertThrows(IllegalArgumentException::class.java) {
            screen.draw(byte, row, col)
        }

        assertEquals("Row or column out of bounds.", exception.message, "Exception message should match")
    }


}
