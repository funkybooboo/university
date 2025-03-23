package regular_expressions.email

import regular_expressions.Invalid
import regular_expressions.State

class Maybe1: State {
    override fun next(char: String): State {
        if (char == " ") return Invalid()
        if (char == "@") return Maybe2()
        return Maybe1()
    }
}