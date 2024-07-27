package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.memory.register.MManager.m

class SwitchMemory(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val currentMValue = m.read()[0].toInt()
        val newMValue = if (currentMValue == 0) {
            1
        } else {
            0
        }
        val newMValueBytes = byteArrayOf(newMValue.toByte())
        m.write(newMValueBytes)
    }
}