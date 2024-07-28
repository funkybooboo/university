package com.natestott.emulator

fun main(args: Array<String>) {
    val computerEmulator = ComputerEmulator()
    if (args.isNotEmpty()) {
        computerEmulator.run(args[0])
    }
    else {
        computerEmulator.run()
    }
}
