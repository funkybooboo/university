package shapes_strategy_pattern.polygon

import shapes_strategy_pattern.Point

interface PolygonPolicy {
    fun isValidOrException(points: List<Point>);
    fun area(points: List<Point>): Double;
}