package regular_expressions.email

import regular_expressions.Invalid
import regular_expressions.State

class Maybe2: State {
    override fun next(char: String): State {
        if (char == " " || char == "@") return Invalid()
        if (char == ".") return Maybe3()
        return Maybe2()
    }
}