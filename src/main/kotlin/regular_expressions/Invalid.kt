package com.natestott.regular_expressions

class Invalid: State {
    override fun next(char: String): State {
        return Invalid()
    }
}