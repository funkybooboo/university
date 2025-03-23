package shapes.curve

import shapes.Point
import shapes.Shape

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