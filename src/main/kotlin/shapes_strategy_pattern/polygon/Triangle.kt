package shapes_strategy_pattern.polygon

import shapes_strategy_pattern.Point
import java.security.InvalidParameterException
import kotlin.math.abs

class Triangle: PolygonPolicy {
    override fun isValidOrException(points: List<Point>) {
        if (points.size != 3) throw InvalidParameterException("Triangles should have three points");
        if (points[0] == points[1] || points[0] == points[2] || points[1] == points[2]) {
            throw IllegalArgumentException("Points must be distinct to form a triangle.")
        }
    }

    override fun area(points: List<Point>): Double =
        abs(points[0].x * (points[1].y - points[2].y) + points[1].x * (points[2].y - points[0].y) + points[2].x * (points[0].y - points[1].y)) / 2;
}
