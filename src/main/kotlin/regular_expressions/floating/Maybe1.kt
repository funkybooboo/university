package regular_expressions.floating

import regular_expressions.Invalid
import regular_expressions.State

class Maybe1: State {
    override fun next(char: String): State {
        if (char == ".") return Maybe2()
        return Invalid()
    }
}