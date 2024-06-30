import shapes.Point
import kotlin.test.Test
import kotlin.test.assertEquals

class PointTest {

    // 2.1.1
    // 2.1.2
    @Test
    fun testConstruction() {
        val x = 1.0;
        val y = 2.0;
        val point = Point(x, y);
        assertEquals(x, point.x);
        assertEquals(y, point.y);
    }

    // 2.1.3
    @Test
    fun testClone() {
        val point = Point(3.0, 4.0);
        val clonedPoint = point.clone();
        assertEquals(point, clonedPoint);
    }

    // 2.1.4
    @Test
    fun testMove() {
        val point = Point(1.0, 2.0);
        point.move(2.0, 3.0);
        assertEquals(3.0, point.x);
        assertEquals(5.0, point.y);
    }
}
