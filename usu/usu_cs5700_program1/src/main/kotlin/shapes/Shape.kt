package shapes

abstract class Shape(
    private val _points: List<Point>
): Moveable {

    val points: List<Point>
        get() = _points.map{ point -> point.clone()};

    abstract fun area(): Double;

    override fun move(deltaX: Double, deltaY: Double) = _points.forEach { point -> point.move(deltaX, deltaY) };
}