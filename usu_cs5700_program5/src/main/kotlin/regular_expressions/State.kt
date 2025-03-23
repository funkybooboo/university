package regular_expressions

interface State {
    fun next(char: String): State; // given a char, I will get you the next state
}