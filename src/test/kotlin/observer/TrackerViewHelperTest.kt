package observer

import org.junit.Assert.*
import subject.Shipment
import kotlin.test.Test
import kotlin.test.assertNotNull

class TrackerViewHelperTest {
    @Test
    fun testConstruction() {
        val trackerViewHelper = TrackerViewHelper()
        assertNotNull(trackerViewHelper)
    }

    @Test
    fun testCanGetShipments() {
        val trackerViewHelper = TrackerViewHelper()

        val shipments = trackerViewHelper.shipments

        assertNotNull(shipments)
    }

    @Test
    fun testNotify() {
        val trackerViewHelper = TrackerViewHelper()
        val shipment = Shipment("1")

        trackerViewHelper.notify(shipment)

        assertTrue(trackerViewHelper.shipments.containsKey(shipment.id))
    }

    @Test
    fun testStartTracking() {
        val trackerViewHelper = TrackerViewHelper()
        val shipment = Shipment("1")

        trackerViewHelper.startTracking(shipment)

        assertTrue(trackerViewHelper.shipments.containsKey(shipment.id))
    }

    @Test
    fun testStopTracking() {
        val trackerViewHelper = TrackerViewHelper()
        val shipment = Shipment("1")

        trackerViewHelper.startTracking(shipment)
        trackerViewHelper.stopTracking(shipment)

        assertFalse(trackerViewHelper.shipments.containsKey(shipment.id))
    }
}