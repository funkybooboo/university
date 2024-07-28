package emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.instruction.Jump
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.PManager
import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.junit.jupiter.api.Assertions.assertEquals
import kotlin.test.Test

class JumpTest {

    @Test
    fun testPerformOperation() {
        val pManager = PManager

        val instruction = Jump(byteArrayOf(0x0A, 0x0B, 0x0C))
        instruction.processNibbles()
        instruction.performOperation()

        assertArrayEquals(instruction.addressBytes, pManager.p.read())
    }

    @Test
    fun testIncrementProgramCounter() {
        val pManager = PManager
        val initialPC = 0x1000
        pManager.p.write(intToByteArray(initialPC))

        val instruction = Jump(byteArrayOf(0x0A, 0x0B, 0x0C))
        instruction.incrementProgramCounter()

        assertEquals(initialPC, byteArrayToInt(pManager.p.read()))
    }
}