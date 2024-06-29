package shapes

import java.security.InvalidParameterException
import kotlin.math.abs

open class Rectangle(
    val point1: Point,
    val point2: Point
): Moveable, Areaable {

    init {
        if (point1.x == point2.x && point1.y == point2.y) {
            throw InvalidParameterException("Points can not be equal.")
        };
    }

    override fun move(deltaX: Double, deltaY: Double) {
        point1.x += deltaX;
        point2.x += deltaX;

        point1.y += deltaY;
        point2.y += deltaY;
    }

    override fun area(): Double = abs(point2.x - point1.x) * abs(point2.y - point1.y);

}
