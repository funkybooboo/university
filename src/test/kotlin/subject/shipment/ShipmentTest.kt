package subject.shipment

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertFalse
import kotlinx.coroutines.runBlocking
import observer.TrackerViewHelper
import subject.update.Created
import subject.update.Location
import subject.update.NoteAdded
import subject.update.Shipped
import kotlin.test.Test
import kotlin.test.assertNotNull
import kotlin.test.assertTrue
import manager.ShipmentFactoryManager.shipmentFactory

class ShipmentTest {
    @Test
    fun testConstruction() {
        val shipmentId1 = "123"
        val shipment1 = shipmentFactory.createShipment(shipmentId1, "standard")
        assertNotNull(shipment1)

        val shipmentId2 = "1234"
        val shipment2 = shipmentFactory.createShipment(shipmentId2, "bulk", mutableListOf(), mutableListOf(), mutableListOf(), mutableListOf(), mutableListOf())
        assertNotNull(shipment2)
    }

    @Test
    fun testCanGetAttributes() {
        val shipmentId = "123"
        val shipment = shipmentFactory.createShipment(shipmentId, "standard")
        assertEquals(shipmentId, shipment!!.id)
    }

    @Test
    fun testAddObserver() {
        val shipmentId = "123"
        val shipment = shipmentFactory.createShipment(shipmentId, "standard")

        val observer = TrackerViewHelper()

        runBlocking {
            shipment!!.addObserver(observer)
        }

        assertTrue(shipment!!.hasObserver(observer))
    }

    @Test
    fun testRemoveObserver() {
        val shipmentId = "123"
        val shipment = shipmentFactory.createShipment(shipmentId, "standard")

        val observer = TrackerViewHelper()

        runBlocking {
            shipment!!.addObserver(observer)
        }
        shipment!!.removeObserver(observer)

        assertFalse(shipment.hasObserver(observer))
    }

    @Test
    fun testNotifyObservers() {
        val shipmentId = "123"
        val shipment = shipmentFactory.createShipment(shipmentId, "standard")

        val observer = TrackerViewHelper()
        runBlocking {
            shipment!!.addObserver(observer)
        }

        shipment!!.addUpdate(Created("created", "standard", shipmentId, System.currentTimeMillis(), null))
        assertEquals(1, shipment.updateHistory.size)
    }

    @Test
    fun testAddUpdate() {
        val shipmentId = "123"
        val shipment = shipmentFactory.createShipment(shipmentId, "standard")

        val updateType = "created"
        val update = Created(updateType, "standard", shipmentId, System.currentTimeMillis(), null)

        shipment!!.addUpdate(update)

        assertEquals(1, shipment.updateHistory.size)
        assertEquals(updateType, shipment.updateHistory.first().newStatus)
    }

    @Test
    fun testAddNote() {
        val shipmentId = "123"
        val shipment = shipmentFactory.createShipment(shipmentId, "standard")

        val updateType = "noteadded"
        val update = NoteAdded(updateType, "standard", shipmentId, System.currentTimeMillis(), "note")

        shipment!!.addUpdate(update)

        assertEquals(1, shipment.notes.size)
        assertEquals("note", shipment.notes[0])
    }

    @Test
    fun testAddLocation() {
        val shipmentId = "123"
        val shipment = shipmentFactory.createShipment(shipmentId, "standard")

        val updateType = "location"
        val update = Location(updateType, "standard", shipmentId, System.currentTimeMillis(), "test")

        shipment!!.addUpdate(update)

        assertEquals(1, shipment.locationHistory.size)
        assertEquals("test", shipment.locationHistory[0])
    }

    @Test
    fun testAddExpectedDeliveryDateTimestamp() {
        val shipmentId = "123"
        val shipment = shipmentFactory.createShipment(shipmentId, "standard")

        val updateType = "shipped"
        val update = Shipped(updateType, "standard", shipmentId, System.currentTimeMillis(), "1233432")

        shipment!!.addUpdate(update)

        assertEquals(1, shipment.expectedDeliveryDateTimestampHistory.size)
    }
}