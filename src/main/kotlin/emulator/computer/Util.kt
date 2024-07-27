package com.natestott.emulator.computer

fun breakByteIntoNibbles(byte: Byte): Pair<Byte, Byte> {
    val unsignedByte = byte.toUByte().toInt()

    val highNibble = (unsignedByte shr 4) and 0x0F

    val lowNibble = unsignedByte and 0x0F

    return Pair(highNibble.toByte(), lowNibble.toByte())
}

fun combineNibblesToByte(highNibble: Byte, lowNibble: Byte): Byte {
    val highNibbleInt = (highNibble.toInt() and 0x0F)
    val lowNibbleInt = (lowNibble.toInt() and 0x0F)

    val combinedByte = (highNibbleInt shl 4) or lowNibbleInt

    return combinedByte.toByte()
}

fun byteArrayToInt(byteArray: ByteArray): Int {
    require(byteArray.size == 2) { "ByteArray must be of size 2." }
    return (byteArray[1].toInt() and 0xFF) or ((byteArray[0].toInt() and 0xFF) shl 8)
}

fun intToByteArray(i: Int): ByteArray {
    require(i in 0..0xFFFF) { "Integer value must be between 0 and 65535." } // Ensure the integer fits in 2 bytes

    val byteArray = ByteArray(2)
    byteArray[0] = ((i shr 8) and 0xFF).toByte() // High byte
    byteArray[1] = (i and 0xFF).toByte() // Low byte
    return byteArray
}