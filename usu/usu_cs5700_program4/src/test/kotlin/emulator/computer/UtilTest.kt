package emulator.computer

import com.natestott.emulator.computer.breakByteIntoNibbles
import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.combineNibblesToByte
import com.natestott.emulator.computer.intToByteArray
import kotlin.test.Test
import kotlin.test.assertEquals

class UtilTest {
    @Test
    fun testBreakByteIntoNibbles() {
        val byte = 0xAB.toByte()
        val (highNibble, lowNibble) = breakByteIntoNibbles(byte)
        assertEquals(0x0A.toByte(), highNibble, "High nibble should be 0x0A")
        assertEquals(0x0B.toByte(), lowNibble, "Low nibble should be 0x0B")
    }

    @Test
    fun testCombineNibblesToByte() {
        val highNibble: Byte = 0x0A
        val lowNibble: Byte = 0x0B
        val combinedByte = combineNibblesToByte(highNibble, lowNibble)
        assertEquals(0xAB.toByte(), combinedByte, "Combined byte should be 0xAB")
    }

    @Test
    fun testByteArrayToInt() {
        val byteArray = byteArrayOf(0x12, 0x34)
        val result = byteArrayToInt(byteArray)
        assertEquals(0x1234, result, "Converted integer should be 0x1234")
    }

    @Test
    fun testIntToByteArray() {
        val i = 0x1234
        val byteArray = intToByteArray(i)
        assertEquals(0x12.toByte(), byteArray[0], "High byte should be 0x12")
        assertEquals(0x34.toByte(), byteArray[1], "Low byte should be 0x34")
    }
}
