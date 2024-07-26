package com.natestott.emulator.computer.memory.contiguous

import com.natestott.emulator.computer.memory.Memory

abstract class ContiguousMemory(
    bytes: ByteArray
): Memory(bytes) {
    abstract fun read(): Byte
    abstract fun write(byte: Byte)
}