package regular_expressions

import regular_expressions.integer.Valid

open class Verifier{
    open fun verify(sequence: String, first: State): Boolean {
        var state = first
        sequence.chunked(1).forEach {
            state = state.next(it)
        }
        return state is Valid
    }
}