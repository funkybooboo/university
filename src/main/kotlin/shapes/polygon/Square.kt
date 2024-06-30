package shapes.polygon

import shapes.Point
import java.security.InvalidParameterException
import kotlin.math.abs

class Square: Rectangle(), PolygonPolicy {
    override fun isValidOrException(points: List<Point>) {
        super.isValidOrException(points);
        if (abs(points[0].x-points[1].x) != abs(points[0].y - points[1].y)) {
            throw InvalidParameterException("Width and Height have to be the same.")
        };
    }
}