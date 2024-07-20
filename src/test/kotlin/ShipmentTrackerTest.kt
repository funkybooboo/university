import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNotNull
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlin.test.Test
import kotlin.test.assertNull
import manager.UpdateFactoryManager.updateFactory
import manager.ShipmentFactoryManager.shipmentFactory

class ShipmentTrackerTest {
    @Test
    fun testConstruction() {

        val channel = Channel<String>()

        val shipmentTracker = ShipmentTracker(channel, updateFactory, shipmentFactory)
        assertNotNull(shipmentTracker)
    }

    @Test
    fun testListen() {

        val channel = Channel<String>()

        val shipmentTracker = ShipmentTracker(channel, updateFactory, shipmentFactory)

        runBlocking {
            val job = launch {
                shipmentTracker.listen()
            }

            val updateInfo = "created,standard,123,1626177123"
            channel.send(updateInfo)

            delay(1000)

            val shipment = shipmentTracker.findShipment("123")
            assertNotNull(shipment)
            assertEquals("123", shipment?.id)

            job.cancelAndJoin()
        }
    }

    @Test
    fun testEmptyUpdate() {
        val channel = Channel<String>()

        val shipmentTracker = ShipmentTracker(channel, updateFactory, shipmentFactory)

        runBlocking {
            val job = launch {
                shipmentTracker.listen()
            }

            // No update in the queue

            delay(1000)

            val shipment = shipmentTracker.findShipment("123")
            assertNull(shipment)

            job.cancelAndJoin()
        }
    }

    @Test
    fun testUnknownUpdateType() {
        val channel = Channel<String>()

        val shipmentTracker = ShipmentTracker(channel, updateFactory, shipmentFactory)

        runBlocking {
            val job = launch {
                shipmentTracker.listen()
            }

            val unknownUpdate = "unknown,123,1626177123"
            channel.send(unknownUpdate)

            delay(1000)

            val shipment = shipmentTracker.findShipment("123")
            assertNull(shipment)

            job.cancelAndJoin()
        }
    }

    @Test
    fun testErrorCreatingUpdate() {
        val channel = Channel<String>()

        val shipmentTracker = ShipmentTracker(channel, updateFactory, shipmentFactory)

        runBlocking {
            val job = launch {
                shipmentTracker.listen()
            }

            val updateInfo = "created,123,1626177123"
            channel.send(updateInfo)

            delay(1000)

            val shipment = shipmentTracker.findShipment("123")
            assertNull(shipment)

            job.cancelAndJoin()
        }
    }
}