package regular_expressions.password

import regular_expressions.Invalid
import regular_expressions.State

class Maybe2: State {
    override fun next(char: String): State {
        if (char == " ") return Invalid()
        if (char in "~`!@#$%^&*()_-=+}{[]|\\\"':;<,>.?/") return Maybe4()
        return Maybe2()
    }
}