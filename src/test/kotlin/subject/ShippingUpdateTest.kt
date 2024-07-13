package subject

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull

class ShippingUpdateTest {
    @Test
    fun testConstruction() {
        val newStatus = "test1"
        val previousStatus = "test2"
        val timestamp = System.currentTimeMillis()

        val shippingUpdate = ShippingUpdate(newStatus, previousStatus, timestamp)
        assertNotNull(shippingUpdate)
    }

    @Test
    fun testCanGetAttributes() {
        val newStatus = "test1"
        val previousStatus = "test2"
        val timestamp = System.currentTimeMillis()

        val shippingUpdate = ShippingUpdate(newStatus, previousStatus, timestamp)

        assertEquals(newStatus, shippingUpdate.newStatus)
        assertEquals(previousStatus, shippingUpdate.previousStatus)
        assertEquals(timestamp, shippingUpdate.timestamp)
    }
}