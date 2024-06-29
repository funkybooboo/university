package shapes

class Point(
    x: Double,
    y: Double
) {
    private var _x: Double = x;
    private var _y: Double = y;

    val x: Double
        get() = _x;

    val y: Double
        get() = _y;

    fun move(deltaX: Double, deltaY: Double) {
        _x += deltaX;
        _y += deltaY;
    }
}
