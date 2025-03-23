package shapes.polygon

import shapes.Point
import shapes.Shape

class Polygon(
    points: List<Point>,
    private val policy: PolygonPolicy
): Shape(points) {
    init {
        policy.isValidOrException(points);
    }

    override fun area(): Double = policy.area(points);
}