package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.computer.memory.register.MManager.m
import com.natestott.emulator.computer.memory.contiguous.RamManager.ram
import com.natestott.emulator.computer.memory.contiguous.RomManager
import com.natestott.emulator.computer.memory.register.AManager.a
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class Read(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        logger.log(Level.INFO, "Executing Read instruction for register R$rxIndex")

        val mByteArray = m.read()
        val isUsingROM = mByteArray[0].toInt() != 0

        logger.log(Level.INFO, "Memory type in use: ${if (isUsingROM) "ROM" else "RAM"}")

        val addressBytes = a.read()
        val address = byteArrayToInt(addressBytes)

        logger.log(Level.INFO, "Address being read from: $address (0x${address.toString(16).uppercase()})")

        val value = if (isUsingROM) {
            RomManager.getRom()!!.read(address)
        } else {
            ram.read(address)
        }

        logger.log(Level.INFO, "Value read from address $address: ${value.toInt()} (0x${value.toUByte().toString(16).uppercase()})")

        rx.write(byteArrayOf(value))

        logger.log(Level.INFO, "Value written to R$rxIndex: ${value.toInt()} (0x${value.toUByte().toString(16).uppercase()})")
    }
}
