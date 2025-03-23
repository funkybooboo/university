package shapes

import java.security.InvalidParameterException
import kotlin.math.pow
import kotlin.math.sqrt

class Line(
    private val _point1: Point,
    private val _point2: Point
) : Moveable {

    init {
        if (_point1 == _point2) {
            throw InvalidParameterException("Points cannot be equal.");
        }
    }

    val point1: Point
        get() = _point1.clone();

    val point2: Point
        get() = _point2.clone();

    fun slope(): Double =
        (_point2.y - _point1.y) / (_point2.x - _point1.x);

    fun length(): Double =
        sqrt((_point2.x - _point1.x).pow(2) + (_point2.y - _point1.y).pow(2));

    override fun move(deltaX: Double, deltaY: Double) {
        _point1.move(deltaX, deltaY);
        _point2.move(deltaX, deltaY);
    }

    override fun toString(): String = "Line(point1=$_point1, point2=$_point2)";
}
