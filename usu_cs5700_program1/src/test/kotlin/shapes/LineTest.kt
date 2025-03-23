package shapes

import java.security.InvalidParameterException
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class LineTest {

    // 2.2.1
    @Test
    fun testConstruction() {
        val point1 = Point(1.0, 2.0);
        val point2 = Point(3.0, 4.0);
        val line = Line(point1, point2);
        assertEquals(point1, line.point1);
        assertEquals(point2, line.point2);
    }

    // 2.2.2
    @Test
    fun testInvalidConstruction() {
        val point = Point(1.0, 2.0);
        assertFailsWith<InvalidParameterException> {
            Line(point, point);
        }
    }

    // 2.2.3
    @Test
    fun testSlope() {
        val point1 = Point(1.0, 2.0);
        val point2 = Point(3.0, 4.0);
        val line = Line(point1, point2);
        assertEquals(1.0, line.slope());
    }

    // 2.2.4
    @Test
    fun testLength() {
        val point1 = Point(1.0, 2.0);
        val point2 = Point(4.0, 6.0);
        val line = Line(point1, point2);
        assertEquals(5.0, line.length());
    }

    // 2.2.5
    @Test
    fun testMove() {
        val point1 = Point(1.0, 2.0);
        val point2 = Point(3.0, 4.0);
        val line = Line(point1, point2);
        line.move(1.0, 1.0);
        assertEquals(Point(2.0, 3.0), line.point1);
        assertEquals(Point(4.0, 5.0), line.point2);
    }
}
