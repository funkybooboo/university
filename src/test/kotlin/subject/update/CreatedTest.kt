package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNull
import kotlin.test.Test
import kotlin.test.assertNotNull

class CreatedTest {
    @Test
    fun testConstruction() {
        val updateType = "created"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Created(updateType, "standard", shipmentId, timeStamp, null)
        assertNotNull(update)
    }

    @Test
    fun testCanGetAttributes() {
        val updateType = "created"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Created(updateType, "standard", shipmentId, timeStamp, null)

        assertEquals(updateType, update.updateType)
        assertEquals(shipmentId, update.shipmentId)
        assertEquals(timeStamp, update.timestampOfUpdate)
    }

    @Test
    fun testGetLocation() {
        val updateType = "created"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Created(updateType, "standard", shipmentId, timeStamp, null)

        val location = update.getLocation()

        assertNull(location)
    }

    @Test
    fun testGetNote() {
        val updateType = "created"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Created(updateType, "standard", shipmentId, timeStamp, null)

        val note = update.getNote()

        assertNull(note)
    }

    @Test
    fun testGetExpectedDeliveryDateTimestamp() {
        val updateType = "created"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()

        val update = Created(updateType, "standard", shipmentId, timeStamp, null)

        val timestamp = update.getExpectedDeliveryDateTimestamp()

        assertNull(timestamp)
    }
}