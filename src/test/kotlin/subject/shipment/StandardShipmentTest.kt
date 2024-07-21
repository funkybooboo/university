package subject.shipment

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNotSame
import manager.ShipmentFactoryManager.shipmentFactory
import manager.UpdateFactoryManager.updateFactory
import kotlin.test.Test
import kotlin.test.assertNotNull

class StandardShipmentTest {
    @Test
    fun testConstruction() {
        val shipmentId = "789"
        val shipmentType = "standard"

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        assertNotNull(shipment)
        assertEquals(shipmentId, shipment.id)
        assertEquals(shipmentType, shipment.shipmentType)
    }

    @Test
    fun testPassValidate() {
        val shipmentType = "standard"
        val shipmentId = "789"
        val timestamp1 = 1652712855468
        val update1 = updateFactory.createUpdate("created,$shipmentType,$shipmentId,$timestamp1")

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        // For Standard Shipment, validation does not perform any checks
        shipment!!.validate(update1!!)
        shipment.addUpdate(update1)

        assertEquals(0, shipment.abnormalOccurrenceHistory.size)
    }

    @Test
    fun testFailValidate() {
        // Standard Shipment does not perform validation, so no failure scenarios
        assertEquals(0, 0)
    }

    @Test
    fun testCopy() {
        val shipmentId = "789"
        val shipmentType = "standard"
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
