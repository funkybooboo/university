package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNull
import kotlin.test.Test
import kotlin.test.assertNotNull

class CanceledTest {
    @Test
    fun testConstruction() {
        val type = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(type, shipmentId, timeStamp, null)
        assertNotNull(update)
    }

    @Test
    fun testCanGetAttributes() {
        val type = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(type, shipmentId, timeStamp, null)

        assertEquals(type, update.type)
        assertEquals(shipmentId, update.shipmentId)
        assertEquals(timeStamp, update.timestampOfUpdate)
    }

    @Test
    fun testGetLocation() {
        val type = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(type, shipmentId, timeStamp, null)

        val location = update.getLocation()

        assertNull(location)
    }

    @Test
    fun testGetNote() {
        val type = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(type, shipmentId, timeStamp, null)

        val note = update.getNote()

        assertNull(note)
    }

    @Test
    fun testGetExpectedDeliveryDateTimestamp() {
        val type = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(type, shipmentId, timeStamp, null)

        val timestamp = update.getExpectedDeliveryDateTimestamp()

        assertEquals(-1, timestamp)
    }
}