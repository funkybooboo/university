package regular_expressions.integer

import regular_expressions.Invalid
import regular_expressions.State

class IntegerFirst: State {
    override fun next(char: String): State {
        if (char in "123456789") {
            return Valid()
        }
        return Invalid()
    }
}