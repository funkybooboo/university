package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNull
import kotlin.test.Test
import kotlin.test.assertNotNull

class CanceledTest {
    @Test
    fun testConstruction() {
        val updateType = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(updateType, null, shipmentId, timeStamp, null)
        assertNotNull(update)
    }

    @Test
    fun testCanGetAttributes() {
        val updateType = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(updateType, null, shipmentId, timeStamp, null)

        assertEquals(updateType, update.updateType)
        assertEquals(shipmentId, update.shipmentId)
        assertEquals(timeStamp, update.timestampOfUpdate)
    }

    @Test
    fun testGetLocation() {
        val updateType = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(updateType, null, shipmentId, timeStamp, null)

        val location = update.getLocation()

        assertNull(location)
    }

    @Test
    fun testGetNote() {
        val updateType = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(updateType, null, shipmentId, timeStamp, null)

        val note = update.getNote()

        assertNull(note)
    }

    @Test
    fun testGetExpectedDeliveryDateTimestamp() {
        val updateType = "canceled"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Canceled(updateType, null, shipmentId, timeStamp, null)

        val timestamp = update.getExpectedDeliveryDateTimestamp()

        assertEquals(-1, timestamp)
    }
}