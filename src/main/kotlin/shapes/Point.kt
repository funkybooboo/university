package shapes

class Point(
    var x: Double,
    var y: Double
): Moveable {
    override fun move(deltaX: Double, deltaY: Double) {
        x += deltaX;
        y += deltaY;
    }

    fun clone(): Point = Point(x, y);
}
