package shapes

class Point(
    x: Double,
    y: Double
): Moveable {
    private var _x: Double = x;
    private var _y: Double = y;

    val x: Double
        get() = _x;

    val y: Double
        get() = _y;

    fun clone(): Point = Point(_x, _y);

    override fun move(deltaX: Double, deltaY: Double) {
        _x += deltaX;
        _y += deltaY;
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true;
        if (javaClass != other?.javaClass) return false;
        other as Point
        return x == other.x && y == other.y;
    }

    override fun hashCode(): Int {
        var result = _x.hashCode()
        result = 31 * result + _y.hashCode()
        return result
    }

    override fun toString(): String {
        return "Point(x=$_x, y=$_y)"
    }
}
