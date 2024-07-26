package com.natestott.emulator.computer.memory.register

import com.natestott.emulator.computer.memory.Memory

abstract class Register(
    bytes: ByteArray
): Memory(bytes) {
    abstract fun read(): ByteArray
    abstract fun write(bytes: ByteArray)
}