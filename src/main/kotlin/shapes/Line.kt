package shapes

import java.security.InvalidParameterException
import kotlin.math.pow
import kotlin.math.sqrt

class Line(
    val point1: Point,
    val point2: Point
): Moveable {

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

    fun slope(): Double = (point2.y - point1.y) / (point2.x - point1.x);

    fun length(): Double = sqrt(((point2.x - point1.x).pow(2)) + ((point2.y - point1.y).pow(2)));
}
