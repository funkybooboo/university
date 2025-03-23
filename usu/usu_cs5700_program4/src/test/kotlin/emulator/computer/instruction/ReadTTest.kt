package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.ReadT
import com.natestott.emulator.computer.memory.register.R
import com.natestott.emulator.computer.memory.register.RManager
import com.natestott.emulator.computer.memory.register.TManager
import kotlin.test.Test
import kotlin.test.assertEquals

class ReadTTest {
    @Test
    fun testProcessNibbles() {
        val rxIndex = 0x2
        val nibbles = byteArrayOf(rxIndex.toByte(), 0x0, 0x0)

        val instruction = ReadT(nibbles)
        instruction.processNibbles()

        assertEquals(RManager.r[rxIndex], instruction.rx)
    }

    @Test
    fun testPerformOperation() {
        val tValue = 0x42.toByte() // Example value in T register

        TManager.t.write(byteArrayOf(tValue))

        val rx = R()
        val rxIndex = 0x2
        val nibbles = byteArrayOf(rxIndex.toByte(), 0x0, 0x0)
        val instruction = ReadT(nibbles)
        instruction.rx = rx

        instruction.performOperation()

        assertEquals(tValue, rx.read()[0])
    }
}
