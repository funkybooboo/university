package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNull
import kotlin.test.Test
import kotlin.test.assertNotNull

class DeliveredTest {
    @Test
    fun testConstruction() {
        val updateType = "delivered"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo: String? = null

        val update = Delivered(updateType, null, shipmentId, timeStamp, otherInfo)
        assertNotNull(update)
    }

    @Test
    fun testCanGetAttributes() {
        val updateType = "delivered"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo: String? = null

        val update = Delivered(updateType, null, shipmentId, timeStamp, otherInfo)

        assertEquals(updateType, update.updateType)
        assertEquals(shipmentId, update.shipmentId)
        assertEquals(timeStamp, update.timestampOfUpdate)
        assertEquals(otherInfo, update.otherInfo)
    }

    @Test
    fun testGetLocation() {
        val updateType = "delivered"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo: String? = null

        val update = Delivered(updateType, null, shipmentId, timeStamp, otherInfo)

        val location = update.getLocation()

        assertNull(location)
    }

    @Test
    fun testGetNote() {
        val updateType = "delivered"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo: String? = null

        val update = Delivered(updateType, null, shipmentId, timeStamp, otherInfo)

        val note = update.getNote()

        assertNull(note)
    }

    @Test
    fun testGetExpectedDeliveryDateTimestamp() {
        val updateType = "delivered"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo: String? = null

        val update = Delivered(updateType, null, shipmentId, timeStamp, otherInfo)

        val expectedTimestamp = update.getExpectedDeliveryDateTimestamp()

        assertEquals(0, expectedTimestamp)
    }
}