package com.natestott.emulator.computer

import com.natestott.emulator.computer.memory.contiguous.Rom

class ControlUnit(
    val rom: Rom
) {
    fun runProgram() {

    }

    private fun getNextBytesFromRom(): ByteArray {
        return ByteArray(4) { 0b1 }
    }
}