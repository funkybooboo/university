package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.memory.contiguous.RamManager.ram
import com.natestott.emulator.computer.memory.contiguous.RomManager
import com.natestott.emulator.computer.memory.register.AManager.a
import com.natestott.emulator.computer.memory.register.MManager.m
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class Write(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        val addressBytes = a.read()
        val address = byteArrayToInt(addressBytes)

        val mByteArray = m.read()
        val isUsingROM = mByteArray[0].toInt() != 0

        val value = rx.read()[0]

        logger.log(Level.INFO, "Performing Write Operation:")
        logger.log(Level.INFO, "Register R$rxIndex contains value: ${value.toUByte()}")
        logger.log(Level.INFO, "Address to write to: $address")
        logger.log(Level.INFO, "Memory Mode - Using ROM: $isUsingROM")

        if (isUsingROM) {
            RomManager.getRom()?.let {
                it.write(address, value)
                logger.log(Level.INFO, "Written value ${value.toUByte()} to ROM address $address")
            } ?: logger.log(Level.WARNING, "ROM is not initialized")
        } else {
            ram.write(address, value)
            logger.log(Level.INFO, "Written value ${value.toUByte()} to RAM address $address")
        }
    }
}
