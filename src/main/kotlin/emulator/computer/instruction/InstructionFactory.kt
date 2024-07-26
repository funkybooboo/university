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
        return instructions[bytes[0].toInt()](byteArrayOf(bytes[1], bytes[2], bytes[3]))
    }
}