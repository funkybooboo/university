package regular_expressions.password

import regular_expressions.Invalid
import regular_expressions.State

class Maybe3: State {
    override fun next(char: String): State {
        if (char == " ") return Invalid()
        if (char in "ABCDEFGHIJKLMNOPQRSTUVWXYZ") return Valid()
        return Maybe3()
    }
}