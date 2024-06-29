package shapes

import kotlin.math.abs;

class Triangle(
    val point1: Point,
    val point2: Point,
    val point3: Point,
): Moveable, Areaable {

    init {
        if (point1 == point2 || point1 == point3 || point2 == point3) {
            throw IllegalArgumentException("Points must be distinct to form a triangle.")
        }
    }

    override fun move(deltaX: Double, deltaY: Double) {
        point1.x += deltaX;
        point2.x += deltaX;
        point3.x += deltaX;

        point1.y += deltaY;
        point2.y += deltaY;
        point3.y += deltaY;
    }

    override fun area(): Double = abs(point1.x * (point2.y - point3.y) + point2.x * (point3.y - point1.y) + point3.x * (point1.y - point2.y)) / 2;

}