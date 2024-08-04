package regular_expressions.email

import regular_expressions.Invalid
import regular_expressions.State

class EmailFirst: State {
    override fun next(char: String): State {
        if (char == " " || char == "@") return Invalid()
        return Maybe1()
    }
}