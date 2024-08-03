package com.natestott.regular_expressions.integer

import com.natestott.regular_expressions.State

class Invalid: State {
    override fun next(char: String): State {
        return Invalid()
    }
}