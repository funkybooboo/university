package com.natestott.regular_expressions

import com.natestott.regular_expressions.binary.BinaryFirst
import com.natestott.regular_expressions.email.EmailFirst
import com.natestott.regular_expressions.floating.FloatFirst
import com.natestott.regular_expressions.integer.IntegerFirst
import com.natestott.regular_expressions.password.PasswordFirst

class FirstStateFactory {
    private val firstStates = mapOf(
        Pair("integer", IntegerFirst()),
        Pair("float", FloatFirst()),
        Pair("binary", BinaryFirst()),
        Pair("email", EmailFirst()),
        Pair("password", PasswordFirst())
    )

    fun createFirstState(type: String): State? {
        return firstStates[type]
    }
}