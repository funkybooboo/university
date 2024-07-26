package com.natestott.emulator.computer.memory.contiguous

class RamManager() {
    val ram = Ram()
}

class Ram(): ContiguousMemory(
    ByteArray(4096)
) {
    override fun read(): Byte {
        TODO("Not yet implemented")
    }

    override fun write(byte: Byte) {
        TODO("Not yet implemented")
    }

}