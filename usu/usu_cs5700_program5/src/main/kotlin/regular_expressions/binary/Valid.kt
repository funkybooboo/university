package regular_expressions.binary

import regular_expressions.Invalid
import regular_expressions.State
import regular_expressions.Valid

class Valid: State, Valid {
    override fun next(char: String): State {
        if (char == "1") return Valid()
        if (char == "0") return Maybe()
        return Invalid()
    }
}