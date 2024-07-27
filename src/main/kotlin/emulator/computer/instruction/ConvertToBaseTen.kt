package com.natestott.emulator.computer.instruction

import com.natestott.emulator.computer.byteArrayToInt
import com.natestott.emulator.computer.memory.contiguous.RamManager.ram
import com.natestott.emulator.computer.memory.contiguous.RomManager
import com.natestott.emulator.computer.memory.register.RManager.r
import com.natestott.emulator.computer.memory.register.AManager.a
import com.natestott.emulator.computer.memory.register.MManager.m
import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

class ConvertToBaseTen(
    nibbles: ByteArray
) : Instruction(nibbles) {
    override fun performOperation() {
        val rxIndex = nibbles[0].toInt()
        val rx = r[rxIndex]

        val address = byteArrayToInt(a.read())

        val value = rx.read()[0].toInt()

        val hundreds = value / 100
        val tens = (value % 100) / 10
        val ones = value % 10

        val mByteArray = m.read()
        val isUsingROM = mByteArray[0].toInt() != 0

        if (isUsingROM) {
            RomManager.getRom()!!.write(address, hundreds.toByte())
            RomManager.getRom()!!.write(address + 1, tens.toByte())
            RomManager.getRom()!!.write(address + 2, ones.toByte())
            logger.log(Level.INFO, "Converted value $value to base ten and stored at ROM address $address: $hundreds, $tens, $ones")
        } else {
            ram.write(address, hundreds.toByte())
            ram.write(address + 1, tens.toByte())
            ram.write(address + 2, ones.toByte())
            logger.log(Level.INFO, "Converted value $value to base ten and stored at RAM address $address: $hundreds, $tens, $ones")
        }
    }
}
