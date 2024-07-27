package com.natestott.emulator.computer.instruction

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

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

    fun createInstruction(nibble0: Byte, nibble1: Byte, nibble2: Byte, nibble3: Byte): Instruction {
        val instructionConstructor = instructions[nibble0.toInt()]

        val instruction = instructionConstructor(byteArrayOf(nibble1, nibble2, nibble3))
        logger.log(Level.INFO, "Created instruction: ${instruction.javaClass.simpleName}")

        return instruction
    }
}
