package regular_expressions.floating

import regular_expressions.Invalid
import regular_expressions.State

class FloatFirst: State {
    override fun next(char: String): State {
        if (char == "0") return Maybe1()
        if (char == ".") return Maybe2()
        if (char in "123456789") return Maybe3()
        return Invalid()
    }
}