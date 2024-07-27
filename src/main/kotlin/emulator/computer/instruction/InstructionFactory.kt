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

    fun createInstruction(bytes: ByteArray): Instruction {
        require(bytes.size == 2) { "ByteArray must contain exactly 2 bytes." }

        val combinedBytes = ((bytes[0].toInt() and 0xFF) shl 8) or (bytes[1].toInt() and 0xFF)

        val nibble0 = (combinedBytes shr 12) and 0x0F
        val nibble1 = (combinedBytes shr 8) and 0x0F
        val nibble2 = (combinedBytes shr 4) and 0x0F
        val nibble3 = combinedBytes and 0x0F

        logger.log(Level.INFO, "Creating instruction with bytes: ${bytes.joinToString(", ")}")
        logger.log(Level.INFO, "Parsed nibbles: $nibble0, $nibble1, $nibble2, $nibble3")

        val instructionConstructor = instructions[nibble0]

        val instruction = instructionConstructor(byteArrayOf(nibble1.toByte(), nibble2.toByte(), nibble3.toByte()))
        logger.log(Level.INFO, "Created instruction: ${instruction.javaClass.simpleName}")

        return instruction
    }
}
