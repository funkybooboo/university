package com.natestott.emulator.computer.instruction

abstract class Instruction(
    protected val bytes: ByteArray
) {
    fun execute() {
        performOperation()
        incrementProgramCounter()
    }

    abstract fun performOperation()
    abstract fun incrementProgramCounter()
}