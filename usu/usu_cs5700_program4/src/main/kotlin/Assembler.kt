package com.natestott

import java.io.File

fun main(args: Array<String>) {
    val bytes = mutableListOf<Byte>()
    val outfile = File(args[1])

    File(args[0]).forEachLine { line ->
        // Remove everything after ';' and trim any leading/trailing whitespace
        val cleanedLine = line.split(';')[0].trim()

        // Ensure that the line is not empty and has at least 4 characters
        if (cleanedLine.length >= 4) {
            bytes.add(cleanedLine.substring(0, 2).lowercase().toInt(16).toByte())
            bytes.add(cleanedLine.substring(2, 4).lowercase().toInt(16).toByte())
        }
    }

    outfile.writeBytes(bytes.toByteArray())
}
