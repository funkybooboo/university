package shapes

import shapes.curve.Circle
import shapes.curve.Curve
import java.security.InvalidParameterException
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class CircleTest {
    // 2.6.1
    @Test
    fun testConstruction() {
        val circle = Curve(listOf(Point(1.0, 1.0)), listOf(2.0), Circle());
        assertEquals(circle.points.size, 1);
        assertEquals(circle.radii.size, 1);
    }

    // 2.6.2
    @Test
    fun testInvalidConstruction() {
        assertFailsWith<InvalidParameterException> {
            Curve(listOf(Point(1.0, 1.0)), listOf(0.0), Circle());
        }
        assertFailsWith<InvalidParameterException> {
            Curve(listOf(Point(1.0, 1.0)), listOf(2.0, 0.0), Circle());
        }
    }

    // 2.6.3
    @Test
    fun testGetAttributes() {
        val points = listOf(Point(1.0, 1.0));
        val radii = listOf(2.0);
        val circle = Curve(points, radii, Circle());
        assertEquals(points, circle.points);
        assertEquals(radii, circle.radii);
    }

    // 2.6.4
    @Test
    fun testArea() {
        val circle = Curve(listOf(Point(1.0, 1.0)), listOf(2.0), Circle());
        assertEquals(Math.PI * 2.0 * 2.0, circle.area());
    }

    // 2.6.5
    @Test
    fun testMove() {
        val circle = Curve(listOf(Point(1.0, 1.0)), listOf(2.0), Circle());
        circle.move(1.0, 1.0);
        assertEquals(Point(2.0, 2.0), circle.points[0]);
        assertEquals(circle.radii, listOf(2.0));
    }
}