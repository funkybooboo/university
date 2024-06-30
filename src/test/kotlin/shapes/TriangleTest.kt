package shapes

import shapes.polygon.Polygon
import shapes.polygon.Triangle
import java.security.InvalidParameterException
import kotlin.test.Test;
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertNotEquals

class TriangleTest {
    // 2.7.1
    // 2.7.2
    @Test
    fun testConstruction() {
        val triangle = Polygon(listOf(Point(-1.0, 1.0), Point(7.0, 2.0), Point(0.0, 3.0)), Triangle());
        assertEquals(triangle.points.size, 3);
        assertNotEquals(triangle.points[0], triangle.points[1]);
        assertNotEquals(triangle.points[0], triangle.points[2]);
        assertNotEquals(triangle.points[1], triangle.points[2]);
    }
    @Test
    fun testInvalidConstruction() {
        assertFailsWith<InvalidParameterException> {
            Polygon(listOf(Point(1.0, 1.0), Point(4.0, 1.0)), Triangle());
        }
        assertFailsWith<InvalidParameterException> {
            Polygon(listOf(Point(1.0, 1.0), Point(1.0, 1.0), Point(2.0, 2.0)), Triangle());
        }
    }

    // 2.7.3
    @Test
    fun testGetPoints() {
        val point1 = Point(1.0, 1.0);
        val point2 = Point(-4.0, 4.0)
        val point3 = Point(10.0, 2.0);
        val triangle = Polygon(listOf(point1, point2, point3), Triangle());
        assertEquals(triangle.points[0], point1);
        assertEquals(triangle.points[1], point2);
        assertEquals(triangle.points[2], point3);
    }

    // 2.7.4
    @Test
    fun testArea() {
        val triangle = Polygon(listOf(Point(1.0, 1.0), Point(4.0, 1.0), Point(2.2, 5.0)), Triangle());
        assertEquals(6.0, triangle.area());
    }

    // 2.7.5
    @Test
    fun testMove() {
        val triangle = Polygon(listOf(Point(1.0, 1.0), Point(3.6, 13.4), Point(-4.0, -45.0)), Triangle());
        triangle.move(1.0, 1.0);
        assertEquals(Point(2.0, 2.0), triangle.points[0]);
        assertEquals(Point(4.6, 14.4), triangle.points[1]);
        assertEquals(Point(-3.0, -44.0), triangle.points[2]);
    }
}