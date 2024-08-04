package regular_expressions.binary

import regular_expressions.Invalid
import regular_expressions.State

class Maybe: State {
    override fun next(char: String): State {
        if (char == "1") return Valid()
        if (char == "0") return Maybe()
        return Invalid()
    }
}