package regular_expressions.integer

import regular_expressions.Invalid
import regular_expressions.State
import regular_expressions.Valid

class Valid: State, Valid {
    override fun next(char: String): State {
        if (char in "0123456789") return Valid()
        return Invalid()
    }
}