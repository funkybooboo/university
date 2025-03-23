package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNull
import kotlin.test.Test
import kotlin.test.assertNotNull

class DelayedTest {
    @Test
    fun testConstruction() {
        val updateType = "delayed"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "1234567890"

        val update = Delayed(updateType, null, shipmentId, timeStamp, otherInfo)
        assertNotNull(update)
    }

    @Test
    fun testCanGetAttributes() {
        val updateType = "delayed"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "1234567890"

        val update = Delayed(updateType, null, shipmentId, timeStamp, otherInfo)

        assertEquals(updateType, update.updateType)
        assertEquals(shipmentId, update.shipmentId)
        assertEquals(timeStamp, update.timestampOfUpdate)
        assertEquals(otherInfo, update.otherInfo)
    }

    @Test
    fun testGetLocation() {
        val updateType = "delayed"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "1234567890"

        val update = Delayed(updateType, null, shipmentId, timeStamp, otherInfo)

        val location = update.getLocation()

        assertNull(location)
    }

    @Test
    fun testGetNote() {
        val updateType = "delayed"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "1234567890"

        val update = Delayed(updateType, null, shipmentId, timeStamp, otherInfo)

        val note = update.getNote()

        assertNull(note)
    }

    @Test
    fun testGetExpectedDeliveryDateTimestamp() {
        val updateType = "delayed"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = "1234567890"

        val update = Delayed(updateType, null, shipmentId, timeStamp, otherInfo)

        val expectedTimestamp = update.getExpectedDeliveryDateTimestamp()

        assertEquals(otherInfo.toLong(), expectedTimestamp)
    }
}