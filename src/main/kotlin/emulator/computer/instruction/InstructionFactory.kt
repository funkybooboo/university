package com.natestott.emulator.computer.instruction

class InstructionFactory {
    private val instructions = arrayOf(
        ::Store,
        ::Add,
        ::Sub,
        ::Read,
        ::Write,
        ::Jump,
        ::ReadKeyboard,
        ::SwitchMemory,
        ::SkipEqual,
        ::SkipNotEqual,
        ::SetA,
        ::SetT,
        ::ReadT,
        ::ConvertToBaseTen,
        ::ConvertByteToAscii,
        ::Draw
    )

    fun createInstruction(bytes: ByteArray): Instruction {
        require(bytes.size == 2) { "ByteArray must contain exactly 2 bytes." }

        val combinedBytes = ((bytes[0].toInt() and 0xFF) shl 8) or (bytes[1].toInt() and 0xFF)

        val nibble0 = (combinedBytes shr 12) and 0x0F
        val nibble1 = (combinedBytes shr 8) and 0x0F
        val nibble2 = (combinedBytes shr 4) and 0x0F
        val nibble3 = combinedBytes and 0x0F

        val instructionConstructor = instructions[nibble0]

        return instructionConstructor(byteArrayOf(nibble1.toByte(), nibble2.toByte(), nibble3.toByte()))
    }

}