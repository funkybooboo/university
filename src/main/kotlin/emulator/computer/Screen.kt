package com.natestott.emulator.computer

import com.natestott.emulator.logger.LoggerManager.logger
import com.natestott.emulator.logger.Logger.Level

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
        logger.log(Level.INFO, "Displaying the screen buffer:")
        for (row in 0 until BUFFER_HEIGHT) {
            for (col in 0 until BUFFER_WIDTH) {
                val index = row * BUFFER_WIDTH + col
                val char = buffer[index].toInt().toChar()
                print(char)
            }
            println()
        }
        println("=".repeat(BUFFER_WIDTH))
        logger.log(Level.INFO, "Screen display updated.")
    }

    fun draw(byte: Byte, row: Byte, col: Byte) {
        val rowIdx = row.toInt()
        val colIdx = col.toInt()
        if (rowIdx in 0 until BUFFER_HEIGHT && colIdx in 0 until BUFFER_WIDTH) {
            val address = rowIdx * BUFFER_WIDTH + colIdx
            buffer[address] = byte
            logger.log(Level.INFO, "Drawing byte ${byte.toUByte()} at row $rowIdx, column $colIdx")
            display()
        } else {
            logger.log(Level.ERROR, "Attempted to draw out of bounds: row $rowIdx, column $colIdx")
            throw IllegalArgumentException("Row or column out of bounds.")
        }
    }
}
