package regular_expressions.email

import regular_expressions.Invalid
import regular_expressions.State

class Maybe3: State {
    override fun next(char: String): State {
        if (char == " " || char == "@" || char == ".") return Invalid()
        return Valid()
    }
}