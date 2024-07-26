package com.natestott.emulator.computer

import com.natestott.emulator.computer.instruction.Instruction
import kotlinx.coroutines.channels.Channel

object ChannelManager {
    val channel = Channel<Instruction>()
}