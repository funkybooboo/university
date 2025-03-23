package regular_expressions.email

import regular_expressions.Invalid
import regular_expressions.State
import regular_expressions.Valid

class Valid: State, Valid {
    override fun next(char: String): State {
        if (char == " " || char == "@" || char == ".") return Invalid()
        return Valid()
    }
}