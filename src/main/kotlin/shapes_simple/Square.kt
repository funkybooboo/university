package shapes_simple

import java.security.InvalidParameterException
import kotlin.math.abs

class Square(point1: Point, point2: Point): Rectangle(point1, point2) {
    init {
        if (abs(point1.x-point2.x) != abs(point1.y - point2.y)) {
            throw InvalidParameterException("Width and Height have to be the same.")
        };
    }
}