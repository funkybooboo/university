package shapes_strategy_pattern.polygon

import shapes_strategy_pattern.Point
import shapes_strategy_pattern.Shape

class Polygon(
    points: List<Point>,
    private val policy: PolygonPolicy
): Shape(points) {
    init {
        policy.isValidOrException(points);
    }

    override fun area(): Double = policy.area(points);
}