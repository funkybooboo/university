package com.natestott.regular_expressions.integer

import com.natestott.regular_expressions.State

class First: State {
    override fun next(char: String): State {
        if (char in "123456789") {
            return Valid()
        }
        return Invalid()
    }
}