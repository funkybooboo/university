package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.intToByteArray
import com.natestott.emulator.computer.memory.register.PManager.p

abstract class Instruction(
    protected val nibbles: ByteArray
) {
    init {
        require(nibbles.size == 3) { "Nibbles array must contain exactly 3 elements." }
    }

    fun execute() {
        performOperation()
        incrementProgramCounter()
    }

    protected open fun incrementProgramCounter() {
        val currentPC = byteArrayToInt(p.read())
        val newPC = currentPC + 2
        p.write(intToByteArray(newPC))
    }

    protected abstract fun performOperation()
}