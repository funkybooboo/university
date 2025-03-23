package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNull
import kotlin.test.Test
import kotlin.test.assertNotNull

class ShippedTest {
    @Test
    fun testConstruction() {
        val updateType = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(updateType, null, shipmentId, timeStamp, timeInfo)
        assertNotNull(update)
    }

    @Test
    fun testCanGetAttributes() {
        val updateType = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(updateType, null, shipmentId, timeStamp, timeInfo)

        assertEquals(updateType, update.updateType)
        assertEquals(shipmentId, update.shipmentId)
        assertEquals(timeStamp, update.timestampOfUpdate)
        assertEquals(timeInfo, update.otherInfo)
    }

    @Test
    fun testGetLocation() {
        val updateType = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(updateType, null, shipmentId, timeStamp, timeInfo)

        val location = update.getLocation()

        assertNull(location)
    }

    @Test
    fun testGetNote() {
        val updateType = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(updateType, null, shipmentId, timeStamp, timeInfo)

        val note = update.getNote()

        assertNull(note)
    }

    @Test
    fun testGetExpectedDeliveryDateTimestamp() {
        val updateType = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(updateType, null, shipmentId, timeStamp, timeInfo)

        val expectedTimestamp = update.getExpectedDeliveryDateTimestamp()

        assertEquals(timeStamp, expectedTimestamp)
    }
}