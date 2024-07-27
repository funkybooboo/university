package com.natestott.emulator.computer.memory.register

import com.natestott.emulator.computer.memory.Memory

abstract class Register(
    bytes: ByteArray
): Memory(bytes) {
    fun read(): ByteArray {
        return bytes.copyOf()
    }

    abstract fun write(bytes: ByteArray)
}