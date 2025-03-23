package shapes.polygon

import shapes.Point
import java.security.InvalidParameterException
import kotlin.math.abs

class Triangle: PolygonPolicy {
    override fun isValidOrException(points: List<Point>) {
        if (points.size != 3) throw InvalidParameterException("Triangles should have three points");
        if (area(points) == 0.0) {
            throw InvalidParameterException("Triangle cannot have an area of 0");
        }
    }

    override fun area(points: List<Point>): Double =
        abs(points[0].x * (points[1].y - points[2].y) + points[1].x * (points[2].y - points[0].y) + points[2].x * (points[0].y - points[1].y)) / 2;
}
