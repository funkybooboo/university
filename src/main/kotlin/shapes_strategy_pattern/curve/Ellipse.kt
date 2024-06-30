package shapes_strategy_pattern.curve

import shapes_strategy_pattern.Point
import java.security.InvalidParameterException
import kotlin.math.PI

open class Ellipse: CurvePolicy {
    override fun isValidOrException(points: List<Point>, radii: List<Double>) {
        if (points.size != 1) throw IllegalArgumentException("A Ellipse has one point");
        if (radii.size != 2) throw IllegalArgumentException("A Ellipse has two radii");
        if (radii[0] == 0.0 || radii[1] == 0.0) throw InvalidParameterException("Radii can not be 0.0");
    }

    override fun area(points: List<Point>, radii: List<Double>): Double = PI * radii[0] * radii[1];
}