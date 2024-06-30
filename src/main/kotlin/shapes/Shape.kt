package shapes

abstract class Shape(
    val points: List<Point>
): Moveable {
    abstract fun area(): Double;

    override fun move(deltaX: Double, deltaY: Double) = points.forEach { point -> point.move(deltaX, deltaY) };
}