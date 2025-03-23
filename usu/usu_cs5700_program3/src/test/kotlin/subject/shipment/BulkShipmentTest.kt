package subject.shipment

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNotSame
import kotlin.test.Test
import manager.ShipmentFactoryManager.shipmentFactory
import manager.UpdateFactoryManager.updateFactory
import kotlin.test.assertNotNull

class BulkShipmentTest {
    @Test
    fun testConstruction() {
        val shipmentId = "123"
        val shipmentType = "bulk"

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        assertNotNull(shipment)
        assertEquals(shipmentId, shipment.id)
        assertEquals(shipmentType, shipment.shipmentType)
    }

    @Test
    fun testPassValidate() {
        val shipmentType = "bulk"
        val shipmentId = "123"
        val timestamp1 = 1652712855468
        val update1 = updateFactory.createUpdate("created,$shipmentType,$shipmentId,$timestamp1")

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        shipment!!.validate(update1!!)
        shipment.addUpdate(update1)

        val timestamp2 = timestamp1 + 1000

        val timestamp3 = timestamp1 + 6 * 24 * 60 * 60 * 1000

        val update2 = updateFactory.createUpdate("shipped,$shipmentId,$timestamp2,$timestamp3")

        shipment.validate(update2!!)
        shipment.addUpdate(update2)

        assertEquals(0, shipment.abnormalOccurrenceHistory.size)
    }

    @Test
    fun testFailValidate() {
        val shipmentType = "bulk"
        val shipmentId = "123"
        val timestamp1 = 1652712855468
        val update1 = updateFactory.createUpdate("created,$shipmentType,$shipmentId,$timestamp1")

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        shipment!!.validate(update1!!)
        shipment.addUpdate(update1)

        val timestamp2 = timestamp1 + 1000

        val timestamp3 = timestamp1 + 1 * 24 * 60 * 60 * 1000

        val update2 = updateFactory.createUpdate("shipped,$shipmentId,$timestamp2,$timestamp3")

        shipment.validate(update2!!)
        shipment.addUpdate(update2)

        assertEquals(1, shipment.abnormalOccurrenceHistory.size)
    }


    @Test
    fun testCopy() {
        val shipmentId = "123"
        val shipmentType = "bulk"
        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        val copy = shipment!!.copy()

        assertNotSame(shipment, copy)

        assertEquals(shipment.id, copy.id)
        assertEquals(shipment.shipmentType, copy.shipmentType)
        assertEquals(shipment.notes, copy.notes)
        assertEquals(shipment.updateHistory, copy.updateHistory)
        assertEquals(shipment.expectedDeliveryDateTimestampHistory, copy.expectedDeliveryDateTimestampHistory)
        assertEquals(shipment.locationHistory, copy.locationHistory)
        assertEquals(shipment.abnormalOccurrenceHistory, copy.abnormalOccurrenceHistory)
    }
}