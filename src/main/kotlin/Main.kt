package com.natestott

import com.natestott.regular_expressions.integer.First
import com.natestott.regular_expressions.Verifier

fun main() {
    val verifier = Verifier()
    println(verifier.verify("12345", First()))
    println(verifier.verify("1242353342645675437543624315245", First()))
    println(verifier.verify("0124324", First()))
    println(verifier.verify("341323a", First()))
    println(verifier.verify("0", First()))
    println(verifier.verify("asdf", First()))
}