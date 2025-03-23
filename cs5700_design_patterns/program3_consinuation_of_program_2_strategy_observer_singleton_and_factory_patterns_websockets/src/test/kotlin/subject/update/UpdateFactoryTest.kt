package subject.update

import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNotNull
import manager.UpdateFactoryManager.updateFactory
import kotlin.test.Test

class UpdateFactoryTest {

    // Test cases for "created" update type
    @Test
    fun testCreateCreatedUpdate_StandardShipment() {
        val info = "created,standard,001,1652712855468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("created", update!!.updateType)
        assertEquals("standard", (update as Created).shipmentType)
        assertEquals("001", update.shipmentId)
        assertEquals(1652712855468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateCreatedUpdate_ExpressShipment() {
        val info = "created,express,002,1652712856468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("created", update!!.updateType)
        assertEquals("express", (update as Created).shipmentType)
        assertEquals("002", update.shipmentId)
        assertEquals(1652712856468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateCreatedUpdate_OvernightShipment() {
        val info = "created,overnight,003,1652712857468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("created", update!!.updateType)
        assertEquals("overnight", (update as Created).shipmentType)
        assertEquals("003", update.shipmentId)
        assertEquals(1652712857468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateCreatedUpdate_BulkShipment() {
        val info = "created,bulk,004,1652712858468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("created", update!!.updateType)
        assertEquals("bulk", (update as Created).shipmentType)
        assertEquals("004", update.shipmentId)
        assertEquals(1652712858468, update.timestampOfUpdate)
    }

    // Test cases for "shipped" update type
    @Test
    fun testCreateShippedUpdate_StandardShipment() {
        val info = "shipped,001,1652712856468,1999934873"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("shipped", update!!.updateType)
        assertEquals("001", (update as Shipped).shipmentId)
        assertEquals(1652712856468, update.timestampOfUpdate)
        assertEquals(1999934873, update.getExpectedDeliveryDateTimestamp())
    }

    @Test
    fun testCreateShippedUpdate_ExpressShipment() {
        val info = "shipped,002,1652712857468,199999495"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("shipped", update!!.updateType)
        assertEquals("002", (update as Shipped).shipmentId)
        assertEquals(1652712857468, update.timestampOfUpdate)
        assertEquals(199999495, update.getExpectedDeliveryDateTimestamp())
    }

    @Test
    fun testCreateShippedUpdate_OvernightShipment() {
        val info = "shipped,003,1652712858468,1652712859468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("shipped", update!!.updateType)
        assertEquals("003", (update as Shipped).shipmentId)
        assertEquals(1652712858468, update.timestampOfUpdate)
        assertEquals(1652712859468, update.getExpectedDeliveryDateTimestamp())
    }

    @Test
    fun testCreateShippedUpdate_BulkShipment() {
        val info = "shipped,004,1652712859468,1652712860468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("shipped", update!!.updateType)
        assertEquals("004", (update as Shipped).shipmentId)
        assertEquals(1652712859468, update.timestampOfUpdate)
        assertEquals(1652712860468, update.getExpectedDeliveryDateTimestamp())
    }

    // Test cases for "location" update type
    @Test
    fun testCreateLocationUpdate_StandardShipment() {
        val info = "location,001,1652712856468,Los Angeles CA"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("location", update!!.updateType)
        assertEquals("001", (update as Location).shipmentId)
        assertEquals(1652712856468, update.timestampOfUpdate)
        assertEquals("Los Angeles CA", update.getLocation())
    }

    @Test
    fun testCreateLocationUpdate_ExpressShipment() {
        val info = "location,002,1652712857468,New York NY"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("location", update!!.updateType)
        assertEquals("002", (update as Location).shipmentId)
        assertEquals(1652712857468, update.timestampOfUpdate)
        assertEquals("New York NY", update.getLocation())
    }

    @Test
    fun testCreateLocationUpdate_OvernightShipment() {
        val info = "location,003,1652712858468,San Francisco CA"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("location", update!!.updateType)
        assertEquals("003", (update as Location).shipmentId)
        assertEquals(1652712858468, update.timestampOfUpdate)
        assertEquals("San Francisco CA", update.getLocation())
    }

    @Test
    fun testCreateLocationUpdate_BulkShipment() {
        val info = "location,004,1652712859468,Seattle WA"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("location", update!!.updateType)
        assertEquals("004", (update as Location).shipmentId)
        assertEquals(1652712859468, update.timestampOfUpdate)
        assertEquals("Seattle WA", update.getLocation())
    }

    // Test cases for "delivered" update type
    @Test
    fun testCreateDeliveredUpdate_StandardShipment() {
        val info = "delivered,001,1652712856468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("delivered", update!!.updateType)
        assertEquals("001", (update as Delivered).shipmentId)
        assertEquals(1652712856468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateDeliveredUpdate_ExpressShipment() {
        val info = "delivered,002,1652712857468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("delivered", update!!.updateType)
        assertEquals("002", (update as Delivered).shipmentId)
        assertEquals(1652712857468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateDeliveredUpdate_OvernightShipment() {
        val info = "delivered,003,1652712858468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("delivered", update!!.updateType)
        assertEquals("003", (update as Delivered).shipmentId)
        assertEquals(1652712858468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateDeliveredUpdate_BulkShipment() {
        val info = "delivered,004,1652712859468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("delivered", update!!.updateType)
        assertEquals("004", (update as Delivered).shipmentId)
        assertEquals(1652712859468, update.timestampOfUpdate)
    }

    // Test cases for "delayed" update type
    @Test
    fun testCreateDelayedUpdate_StandardShipment() {
        val info = "delayed,001,1652712856468,1999934873"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("delayed", update!!.updateType)
        assertEquals("001", (update as Delayed).shipmentId)
        assertEquals(1652712856468, update.timestampOfUpdate)
        assertEquals(1999934873, update.otherInfo!!.toLong())
    }

    @Test
    fun testCreateDelayedUpdate_ExpressShipment() {
        val info = "delayed,002,1652712857468,199999495"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("delayed", update!!.updateType)
        assertEquals("002", (update as Delayed).shipmentId)
        assertEquals(1652712857468, update.timestampOfUpdate)
        assertEquals(199999495, update.otherInfo!!.toLong())
    }

    @Test
    fun testCreateDelayedUpdate_OvernightShipment() {
        val info = "delayed,003,1652712858468,1652712859468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("delayed", update!!.updateType)
        assertEquals("003", (update as Delayed).shipmentId)
        assertEquals(1652712858468, update.timestampOfUpdate)
        assertEquals(1652712859468, update.otherInfo!!.toLong())
    }

    @Test
    fun testCreateDelayedUpdate_BulkShipment() {
        val info = "delayed,004,1652712859468,1652712860468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("delayed", update!!.updateType)
        assertEquals("004", (update as Delayed).shipmentId)
        assertEquals(1652712859468, update.timestampOfUpdate)
        assertEquals(1652712860468, update.otherInfo!!.toLong())
    }

    // Test cases for "lost" update type
    @Test
    fun testCreateLostUpdate_StandardShipment() {
        val info = "lost,001,1652712856468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("lost", update!!.updateType)
        assertEquals("001", (update as Lost).shipmentId)
        assertEquals(1652712856468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateLostUpdate_ExpressShipment() {
        val info = "lost,002,1652712857468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("lost", update!!.updateType)
        assertEquals("002", (update as Lost).shipmentId)
        assertEquals(1652712857468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateLostUpdate_OvernightShipment() {
        val info = "lost,003,1652712858468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("lost", update!!.updateType)
        assertEquals("003", (update as Lost).shipmentId)
        assertEquals(1652712858468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateLostUpdate_BulkShipment() {
        val info = "lost,004,1652712859468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("lost", update!!.updateType)
        assertEquals("004", (update as Lost).shipmentId)
        assertEquals(1652712859468, update.timestampOfUpdate)
    }

    // Test cases for "canceled" update type
    @Test
    fun testCreateCanceledUpdate_StandardShipment() {
        val info = "canceled,001,1652712856468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("canceled", update!!.updateType)
        assertEquals("001", (update as Canceled).shipmentId)
        assertEquals(1652712856468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateCanceledUpdate_ExpressShipment() {
        val info = "canceled,002,1652712857468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("canceled", update!!.updateType)
        assertEquals("002", (update as Canceled).shipmentId)
        assertEquals(1652712857468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateCanceledUpdate_OvernightShipment() {
        val info = "canceled,003,1652712858468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("canceled", update!!.updateType)
        assertEquals("003", (update as Canceled).shipmentId)
        assertEquals(1652712858468, update.timestampOfUpdate)
    }

    @Test
    fun testCreateCanceledUpdate_BulkShipment() {
        val info = "canceled,004,1652712859468"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("canceled", update!!.updateType)
        assertEquals("004", (update as Canceled).shipmentId)
        assertEquals(1652712859468, update.timestampOfUpdate)
    }

    // Test cases for "noteadded" update type
    @Test
    fun testCreateNoteAddedUpdate_StandardShipment() {
        val info = "noteadded,001,1652712856468,packaging was damaged slightly during shipping"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("noteadded", update!!.updateType)
        assertEquals("001", (update as NoteAdded).shipmentId)
        assertEquals(1652712856468, update.timestampOfUpdate)
        assertEquals("packaging was damaged slightly during shipping", update.otherInfo)
    }

    @Test
    fun testCreateNoteAddedUpdate_ExpressShipment() {
        val info = "noteadded,002,1652712857468,box was opened for inspection"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("noteadded", update!!.updateType)
        assertEquals("002", (update as NoteAdded).shipmentId)
        assertEquals(1652712857468, update.timestampOfUpdate)
        assertEquals("box was opened for inspection", update.otherInfo)
    }

    @Test
    fun testCreateNoteAddedUpdate_OvernightShipment() {
        val info = "noteadded,003,1652712858468,fragile content inside handle with care"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("noteadded", update!!.updateType)
        assertEquals("003", (update as NoteAdded).shipmentId)
        assertEquals(1652712858468, update.timestampOfUpdate)
        assertEquals("fragile content inside handle with care", update.getNote())
    }

    @Test
    fun testCreateNoteAddedUpdate_BulkShipment() {
        val info = "noteadded,004,1652712859468,additional items included"

        val update = updateFactory.createUpdate(info)

        assertNotNull(update)
        assertEquals("noteadded", update!!.updateType)
        assertEquals("004", (update as NoteAdded).shipmentId)
        assertEquals(1652712859468, update.timestampOfUpdate)
        assertEquals("additional items included", update.otherInfo)
    }
}
