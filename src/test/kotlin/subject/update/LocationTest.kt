package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNull
import kotlin.test.Test
import kotlin.test.assertNotNull

class LocationTest {
    @Test
    fun testConstruction() {
        val updateType = "location"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "New York, USA"

        val update = Location(updateType, null, shipmentId, timeStamp, otherInfo)
        assertNotNull(update)
    }

    @Test
    fun testCanGetAttributes() {
        val updateType = "location"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "New York, USA"

        val update = Location(updateType, null, shipmentId, timeStamp, otherInfo)

        assertEquals(updateType, update.updateType)
        assertEquals(shipmentId, update.shipmentId)
        assertEquals(timeStamp, update.timestampOfUpdate)
        assertEquals(otherInfo, update.otherInfo)
    }

    @Test
    fun testGetLocation() {
        val updateType = "location"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "New York, USA"

        val update = Location(updateType, null, shipmentId, timeStamp, otherInfo)

        val location = update.getLocation()

        assertEquals(otherInfo, location)
    }

    @Test
    fun testGetNote() {
        val updateType = "location"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "New York, USA"

        val update = Location(updateType, null, shipmentId, timeStamp, otherInfo)

        val note = update.getNote()

        assertNull(note)
    }

    @Test
    fun testGetExpectedDeliveryDateTimestamp() {
        val updateType = "location"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "New York, USA"

        val update = Location(updateType, null, shipmentId, timeStamp, otherInfo)

        val expectedTimestamp = update.getExpectedDeliveryDateTimestamp()

        assertNull(expectedTimestamp)
    }
}