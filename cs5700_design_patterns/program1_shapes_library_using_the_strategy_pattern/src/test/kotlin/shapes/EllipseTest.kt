package shapes

import shapes.curve.Curve
import shapes.curve.Ellipse
import java.security.InvalidParameterException
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class EllipseTest {
    // 2.5.1
    @Test
    fun testConstruction() {
        val ellipse = Curve(listOf(Point(1.0, 1.0)), listOf(2.0, 1.3), Ellipse());
        assertEquals(ellipse.points.size, 1);
        assertEquals(ellipse.radii.size, 2);
    }

    // 2.5.2
    @Test
    fun testInvalidConstruction() {
        assertFailsWith<InvalidParameterException> {
            Curve(listOf(Point(1.0, 1.0)), listOf(0.0, 1.3), Ellipse());
        }
        assertFailsWith<InvalidParameterException> {
            Curve(listOf(Point(1.0, 1.0)), listOf(2.0, 0.0), Ellipse());
        }
    }

    // 2.5.3
    @Test
    fun testGetAttributes() {
        val points = listOf(Point(1.0, 1.0));
        val radii = listOf(2.0, 1.3);
        val ellipse = Curve(points, radii, Ellipse());
        assertEquals(points, ellipse.points);
        assertEquals(radii, ellipse.radii);
    }

    // 2.5.4
    @Test
    fun testArea() {
        val ellipse = Curve(listOf(Point(1.0, 1.0)), listOf(2.0, 1.3), Ellipse());
        assertEquals(Math.PI * 2.0 * 1.3, ellipse.area());
    }

    // 2.5.5
    @Test
    fun testMove() {
        val ellipse = Curve(listOf(Point(1.0, 1.0)), listOf(2.0, 1.3), Ellipse());
        ellipse.move(1.0, 1.0);
        assertEquals(Point(2.0, 2.0), ellipse.points[0]);
        assertEquals(ellipse.radii, listOf(2.0, 1.3));
    }
}