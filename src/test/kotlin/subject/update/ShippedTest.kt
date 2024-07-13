package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNull
import kotlin.test.Test
import kotlin.test.assertNotNull

class ShippedTest {
    @Test
    fun testConstruction() {
        val type = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(type, shipmentId, timeStamp, timeInfo)
        assertNotNull(update)
    }

    @Test
    fun testCanGetAttributes() {
        val type = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(type, shipmentId, timeStamp, timeInfo)

        assertEquals(type, update.type)
        assertEquals(shipmentId, update.shipmentId)
        assertEquals(timeStamp, update.timestampOfUpdate)
        assertEquals(timeInfo, update.otherInfo)
    }

    @Test
    fun testGetLocation() {
        val type = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(type, shipmentId, timeStamp, timeInfo)

        val location = update.getLocation()

        assertNull(location)
    }

    @Test
    fun testGetNote() {
        val type = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(type, shipmentId, timeStamp, timeInfo)

        val note = update.getNote()

        assertNull(note)
    }

    @Test
    fun testGetExpectedDeliveryDateTimestamp() {
        val type = "shipped"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val timeInfo = timeStamp.toString()

        val update = Shipped(type, shipmentId, timeStamp, timeInfo)

        val expectedTimestamp = update.getExpectedDeliveryDateTimestamp()

        assertEquals(timeStamp, expectedTimestamp)
    }
}