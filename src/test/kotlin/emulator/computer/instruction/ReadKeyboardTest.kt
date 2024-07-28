package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.ReadKeyboard
import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import kotlin.test.Test
import kotlin.test.assertEquals

class ReadKeyboardTest {
    @Test
    fun testProcessNibbles() {
        val rxIndex = 0x2
        val nibbles = byteArrayOf(rxIndex.toByte(), 0x0, 0x0)

        val instruction = ReadKeyboard(nibbles)
        instruction.processNibbles()

        assertEquals(RManager.r[rxIndex], instruction.rx)
    }

    @Test
    fun testPerformOperation() {
        val initialInput = "1F"
        val expectedByte = 0x1F.toByte()

        val originalInput = System.`in`
        val originalOutput = System.out
        val inputContent = ByteArrayInputStream(initialInput.toByteArray())
        System.setIn(inputContent)
        val outputContent = ByteArrayOutputStream()
        System.setOut(PrintStream(outputContent))

        val instruction = ReadKeyboard(byteArrayOf(0x0, 0x0, 0x0))
        instruction.rx = R()

        instruction.performOperation()

        System.setIn(originalInput)
        System.setOut(originalOutput)

        assertEquals(expectedByte, instruction.rx.read()[0])
    }
}
