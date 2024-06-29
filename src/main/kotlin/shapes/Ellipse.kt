package shapes

import java.security.InvalidParameterException
import kotlin.math.PI

open class Ellipse(
    val point: Point,
    val radius1: Double,
    val radius2: Double
): Moveable, Areaable {


    init {
        if (radius1 == 0.0 || radius2 == 0.0) throw InvalidParameterException("Radii can not be 0.0");
    }

    override fun move(deltaX: Double, deltaY: Double) {
        point.move(deltaX, deltaY);
    }

    override fun area(): Double = PI * radius1 * radius2;
}