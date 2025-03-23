package com.natestott.emulator.computer

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

fun breakByteIntoNibbles(byte: Byte): Pair<Byte, Byte> {
    val unsignedByte = byte.toUByte().toInt()
    val highNibble = (unsignedByte shr 4) and 0x0F
    val lowNibble = unsignedByte and 0x0F
    logger.log(Level.INFO, "Breaking byte $unsignedByte into nibbles: highNibble=$highNibble, lowNibble=$lowNibble")
    return Pair(highNibble.toByte(), lowNibble.toByte())
}

fun combineNibblesToByte(highNibble: Byte, lowNibble: Byte): Byte {
    val highNibbleInt = (highNibble.toInt() and 0x0F)
    val lowNibbleInt = (lowNibble.toInt() and 0x0F)
    val combinedByte = (highNibbleInt shl 4) or lowNibbleInt
    logger.log(Level.INFO, "Combining nibbles: highNibble=${highNibble.toUByte()}, lowNibble=${lowNibble.toUByte()}, combinedByte=${combinedByte.toUByte()}")
    return combinedByte.toByte()
}

fun byteArrayToInt(byteArray: ByteArray): Int {
    require(byteArray.size == 2) { "ByteArray must be of size 2." }
    val result = (byteArray[1].toInt() and 0xFF) or ((byteArray[0].toInt() and 0xFF) shl 8)
    logger.log(Level.INFO, "Converting byteArray ${byteArray.joinToString(", ")} to Int: result=$result")
    return result
}

fun intToByteArray(i: Int): ByteArray {
    require(i in 0..0xFFFF) { "Integer value must be between 0 and 65535." }
    val byteArray = ByteArray(2)
    byteArray[0] = ((i shr 8) and 0xFF).toByte() // High byte
    byteArray[1] = (i and 0xFF).toByte() // Low byte
    logger.log(Level.INFO, "Converting Int $i to byteArray: ${byteArray.joinToString(", ")}")
    return byteArray
}
