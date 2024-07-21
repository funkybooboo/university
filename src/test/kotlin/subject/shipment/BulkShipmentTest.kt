package subject.shipment

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNotSame
import kotlin.test.Test
import manager.ShipmentFactoryManager.shipmentFactory
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