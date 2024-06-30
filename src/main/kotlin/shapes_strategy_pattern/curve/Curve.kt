package shapes_strategy_pattern.curve

import shapes_strategy_pattern.Point
import shapes_strategy_pattern.Shape

class Curve(
    points: List<Point>,
    val radii: List<Double>,
    private val policy: CurvePolicy
): Shape(points) {
    init {
        policy.isValidOrException(points, radii);
    }

    override fun area(): Double = policy.area(points, radii);
}