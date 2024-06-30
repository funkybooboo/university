package shapes.curve

import shapes.Point
import java.security.InvalidParameterException
import kotlin.math.PI

open class Ellipse: CurvePolicy {
    override fun isValidOrException(points: List<Point>, radii: List<Double>) {
        if (points.size != 1) throw InvalidParameterException("A Ellipse has one point");
        if (radii.size != 2) throw InvalidParameterException("A Ellipse has two radii");
        if (area(points, radii) == 0.0) throw InvalidParameterException("Area cannot be 0");
    }

    override fun area(points: List<Point>, radii: List<Double>): Double = PI * radii[0] * radii[1];
}