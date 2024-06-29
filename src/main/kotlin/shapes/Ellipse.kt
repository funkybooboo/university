package shapes

import java.security.InvalidParameterException
import kotlin.math.PI

open class Ellipse(
    val point: Point,
    var radius1: Double,
    var radius2: Double
): Moveable, Areaable {

    init {
        if (radius1 == 0.0 || radius2 == 0.0) throw InvalidParameterException("Radii can not be 0.0");
    }

    override fun move(deltaX: Double, deltaY: Double) {
        point.x += deltaX;
        point.y += deltaY;
    }

    override fun area(): Double = PI * radius1 * radius2;
}