package com.natestott.emulator.computer

import com.natestott.emulator.computer.instruction.Instruction
import kotlinx.coroutines.channels.Channel

class ChannelManager() {
    val channel = Channel<Instruction>()
}