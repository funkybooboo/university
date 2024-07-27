package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.memory.contiguous.RamManager.ram
import com.natestott.emulator.computer.memory.contiguous.RomManager
import com.natestott.emulator.computer.memory.register.AManager.a
import com.natestott.emulator.computer.memory.register.MManager.m
import com.natestott.emulator.computer.memory.register.RManager.r

class Write(
    nibbles: ByteArray
): Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        val addressBytes = a.read()
        val address = byteArrayToInt(addressBytes)

        val mByteArray = m.read()
        val isUsingROM = mByteArray[0].toInt() != 0

        val value = rx.read()[0]

        if (isUsingROM) {
            RomManager.getRom()!!.write(address, value)
        } else {
            ram.write(address, value)
        }
    }
}