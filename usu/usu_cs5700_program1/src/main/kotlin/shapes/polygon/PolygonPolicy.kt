package shapes.polygon

import shapes.Point

interface PolygonPolicy {
    fun isValidOrException(points: List<Point>);
    fun area(points: List<Point>): Double;
}