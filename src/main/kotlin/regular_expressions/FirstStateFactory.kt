package regular_expressions

import regular_expressions.binary.BinaryFirst
import regular_expressions.email.EmailFirst
import regular_expressions.floating.FloatFirst
import regular_expressions.integer.IntegerFirst
import regular_expressions.password.PasswordFirst

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