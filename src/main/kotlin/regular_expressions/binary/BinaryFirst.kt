package regular_expressions.binary

import regular_expressions.Invalid
import regular_expressions.State

class BinaryFirst: State {
    override fun next(char: String): State {
        if (char == "1") return Valid()
        return Invalid()
    }
}