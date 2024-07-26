package com.natestott.emulator.computer

import com.natestott.emulator.computer.instruction.Instruction
import com.natestott.emulator.computer.memory.contiguous.Rom
import kotlinx.coroutines.channels.Channel

class ControlUnit(
    val rom: Rom,
    private val channel: Channel<Instruction>
) {
    fun runProgram() {

    }

    private fun getNextBytesFromRom(): ByteArray {
        return ByteArray(4) { 0b1 }
    }
}