package shapes

import shapes.polygon.Polygon
import shapes.polygon.Rectangle
import java.security.InvalidParameterException
import kotlin.test.Test;
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertNotEquals

class RectangleTest {
    // 2.3.1
    @Test
    fun testConstruction() {
        val rectangle = Polygon(listOf(Point(1.0, 1.0), Point(2.0, 2.0)), Rectangle());
        assertEquals(rectangle.points.size, 2);
        assertNotEquals(rectangle.points[0], rectangle.points[1]);
    }

    // 2.3.2
    @Test
    fun testInvalidConstruction() {
        assertFailsWith<InvalidParameterException> {
            Polygon(listOf(Point(1.0, 1.0), Point(1.0, 1.0)), Rectangle());
        }
    }

    // 2.3.3
    @Test
    fun testGetPoints() {
        val point1 = Point(1.0, 1.0);
        val point2 = Point(2.0, 2.0);
        val rectangle = Polygon(listOf(point1, point2), Rectangle());
        assertEquals(rectangle.points[0], point1);
        assertEquals(rectangle.points[1], point2);
    }

    // 2.3.4
    @Test
    fun testArea() {
        val rectangle = Polygon(listOf(Point(1.0, 1.0), Point(3.0, 4.0)), Rectangle());
        assertEquals(6.0, rectangle.area());
    }

    // 2.3.5
    @Test
    fun testMove() {
        val rectangle = Polygon(listOf(Point(1.0, 1.0), Point(3.0, 4.0)), Rectangle());
        rectangle.move(1.0, 1.0);
        assertEquals(Point(2.0, 2.0), rectangle.points[0]);
        assertEquals(Point(4.0, 5.0), rectangle.points[1]);
    }
}