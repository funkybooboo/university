package com.natestott.emulator.computer

import com.natestott.emulator.computer.instruction.Instruction
import kotlinx.coroutines.channels.Channel

class Cpu(
    private val channel: Channel<Instruction>
) {
    fun run() {

    }

    fun incrementT() {

    }
}