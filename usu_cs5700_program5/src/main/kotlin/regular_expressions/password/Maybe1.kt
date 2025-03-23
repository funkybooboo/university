package regular_expressions.password

import regular_expressions.Invalid
import regular_expressions.State

class Maybe1: State {
    override fun next(char: String): State {
        if (char == " ") return Invalid()
        if (char in "ABCDEFGHIJKLMNOPQRSTUVWXYZ") return Maybe2()
        if (char in "~`!@#$%^&*()_-=+}{[]|\\\"':;<,>.?/") return Maybe3()
        return Maybe1()
    }
}