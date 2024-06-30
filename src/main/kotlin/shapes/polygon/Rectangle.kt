package shapes.polygon

import shapes.Point
import java.security.InvalidParameterException
import kotlin.math.abs

open class Rectangle: PolygonPolicy {
    override fun isValidOrException(points: List<Point>) {
        if (points.size != 2) throw InvalidParameterException("Rectangle should only have two points");
        if (points[0].x == points[1].x && points[0].y == points[1].y) {
            throw InvalidParameterException("Points can not be equal.");
        };
    }

    override fun area(points: List<Point>): Double = abs(points[1].x - points[0].x) * abs(points[1].y - points[0].y);
}