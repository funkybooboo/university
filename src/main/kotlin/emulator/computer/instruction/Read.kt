package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.computer.memory.register.MManager.m
import com.natestott.emulator.computer.memory.contiguous.RamManager.ram
import com.natestott.emulator.computer.memory.contiguous.RomManager
import com.natestott.emulator.computer.memory.register.AManager.a

class Read(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        val mByteArray = m.read()
        val isUsingROM = mByteArray[0].toInt() != 0

        val addressBytes = a.read()
        val address = byteArrayToInt(addressBytes)

        val value = if (isUsingROM) {
            RomManager.getRom()!!.read(address)
        } else {
            ram.read(address)
        }

        rx.write(byteArrayOf(value))
    }
}
