package subject.shipment

import junit.framework.TestCase.assertEquals
import manager.ShipmentFactoryManager.shipmentFactory
import kotlin.test.Test

class ShipmentFactoryTest {
    @Test
    fun testCreateExpressShipment() {
        val shipmentId = "001"
        val shipmentType = "express"

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        assertEquals(ExpressShipment::class.java, shipment!!::class.java)
        assertEquals(shipmentId, shipment.id)
        assertEquals(shipmentType, shipment.shipmentType)
    }

    @Test
    fun testCreateOvernightShipment() {
        val shipmentId = "002"
        val shipmentType = "overnight"

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        assertEquals(OvernightShipment::class.java, shipment!!::class.java)
        assertEquals(shipmentId, shipment.id)
        assertEquals(shipmentType, shipment.shipmentType)
    }

    @Test
    fun testCreateBulkShipment() {
        val shipmentId = "003"
        val shipmentType = "bulk"

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        assertEquals(BulkShipment::class.java, shipment!!::class.java)
        assertEquals(shipmentId, shipment.id)
        assertEquals(shipmentType, shipment.shipmentType)
    }

    @Test
    fun testCreateStandardShipment() {
        val shipmentId = "004"
        val shipmentType = "standard"

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        assertEquals(StandardShipment::class.java, shipment!!::class.java)
        assertEquals(shipmentId, shipment.id)
        assertEquals(shipmentType, shipment.shipmentType)
    }

    @Test
    fun testInvalidShipmentType() {
        val shipmentId = "005"
        val shipmentType = "invalid"

        val shipment = shipmentFactory.createShipment(shipmentId, shipmentType)

        assertEquals(null, shipment)
    }
}
