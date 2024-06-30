package shapes.curve

import shapes.Point

interface CurvePolicy {
    fun isValidOrException(points: List<Point>, radii: List<Double>);
    fun area(points: List<Point>, radii: List<Double>): Double;
}