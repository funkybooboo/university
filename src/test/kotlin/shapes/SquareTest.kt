package shapes

import shapes.polygon.Polygon
import shapes.polygon.Square
import java.security.InvalidParameterException
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertNotEquals

class SquareTest {
    // 2.4.1
    // 2.4.2
    @Test
    fun testConstruction() {
        val square = Polygon(listOf(Point(1.0, 1.0), Point(2.0, 2.0)), Square());
        assertEquals(square.points.size, 2);
        assertNotEquals(square.points[0], square.points[1]);
    }
    @Test
    fun testInvalidConstruction() {
        assertFailsWith<InvalidParameterException> {
            Polygon(listOf(Point(1.0, 1.0), Point(1.0, 1.0)), Square());
        }
        assertFailsWith<InvalidParameterException> {
            Polygon(listOf(Point(1.0, 1.0), Point(5.0, 1.0)), Square());
        }
    }

    // 2.4.3
    @Test
    fun testGetPoints() {
        val point1 = Point(1.0, 1.0);
        val point2 = Point(4.0, 4.0);
        val square = Polygon(listOf(point1, point2), Square());
        assertEquals(square.points[0], point1);
        assertEquals(square.points[1], point2);
    }

    // 2.4.4
    @Test
    fun testArea() {
        val square = Polygon(listOf(Point(1.0, 1.0), Point(3.0, 3.0)), Square());
        assertEquals(4.0, square.area());
    }

    // 2.4.5
    @Test
    fun testMove() {
        val square = Polygon(listOf(Point(1.0, 1.0), Point(3.0, 3.0)), Square());
        square.move(1.0, 1.0);
        assertEquals(Point(2.0, 2.0), square.points[0]);
        assertEquals(Point(4.0, 4.0), square.points[1]);
    }

}