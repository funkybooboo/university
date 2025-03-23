package regular_expressions.password

import regular_expressions.Invalid
import regular_expressions.State

class PasswordFirst: State {
    override fun next(char: String): State {
        if (char == " ") return Invalid()
        return Maybe1()
    }
}