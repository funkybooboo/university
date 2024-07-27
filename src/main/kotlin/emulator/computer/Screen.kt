package com.natestott.emulator.computer

object ScreenManager {
    val screen = Screen()
}

class Screen {
    companion object {
        const val BUFFER_WIDTH = 8
        const val BUFFER_HEIGHT = 8
    }

    private val buffer: ByteArray = ByteArray(BUFFER_WIDTH * BUFFER_HEIGHT)

    private fun display() {
        for (row in 0 until BUFFER_HEIGHT) {
            for (col in 0 until BUFFER_WIDTH) {
                val index = row * BUFFER_WIDTH + col
                val char = buffer[index].toInt().toChar()
                print(char)
            }
            println()
        }
        println("============")
    }

    fun draw(byte: Byte, row: Byte, col: Byte) {
        val rowIdx = row.toInt()
        val colIdx = col.toInt()
        if (rowIdx in 0 until BUFFER_HEIGHT && colIdx in 0 until BUFFER_WIDTH) {
            val address = rowIdx * BUFFER_WIDTH + colIdx
            buffer[address] = byte
        } else {
            throw IllegalArgumentException("Row or column out of bounds.")
        }
        display()
    }
}
