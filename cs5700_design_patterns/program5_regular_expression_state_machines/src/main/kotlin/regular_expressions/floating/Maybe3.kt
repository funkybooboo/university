package regular_expressions.floating

import regular_expressions.Invalid
import regular_expressions.State

class Maybe3: State {
    override fun next(char: String): State {
        if (char == ".") return Maybe2()
        if (char in "0123456789") return Maybe3()
        return Invalid()
    }
}