package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNull
import kotlin.test.Test
import kotlin.test.assertNotNull

class LostTest {
    @Test
    fun testConstruction() {
        val type = "lost"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = null

        val update = Lost(type, shipmentId, timeStamp, otherInfo)
        assertNotNull(update)
    }

    @Test
    fun testCanGetAttributes() {
        val type = "lost"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = null

        val update = Lost(type, shipmentId, timeStamp, otherInfo)

        assertEquals(type, update.type)
        assertEquals(shipmentId, update.shipmentId)
        assertEquals(timeStamp, update.timestampOfUpdate)
        assertEquals(otherInfo, update.otherInfo)
    }

    @Test
    fun testGetLocation() {
        val type = "lost"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = null

        val update = Lost(type, shipmentId, timeStamp, otherInfo)

        val location = update.getLocation()

        assertEquals("", location)
    }

    @Test
    fun testGetNote() {
        val type = "lost"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = null

        val update = Lost(type, shipmentId, timeStamp, otherInfo)

        val note = update.getNote()

        assertNull(note)
    }

    @Test
    fun testGetExpectedDeliveryDateTimestamp() {
        val type = "lost"
        val shipmentId = "123"
        val timeStamp = System.currentTimeMillis()
        val otherInfo = null

        val update = Lost(type, shipmentId, timeStamp, otherInfo)

        val expectedTimestamp = update.getExpectedDeliveryDateTimestamp()

        assertEquals(-1, expectedTimestamp)
    }
}