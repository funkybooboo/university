import junit.framework.TestCase.assertEquals
import junit.framework.TestCase.assertNotNull
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import listener.Queue
import subject.Shipment
import subject.update.*
import kotlin.test.Test
import kotlin.test.assertNull

class ShipmentTrackerTest {
    @Test
    fun testConstruction() {
        val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
            Pair("created", ::Created),
            Pair("shipped", ::Shipped),
            Pair("location", ::Location),
            Pair("delivered", ::Delivered),
            Pair("delayed", ::Delayed),
            Pair("lost", ::Lost),
            Pair("canceled", ::Canceled),
            Pair("noteadded", ::NoteAdded),
        )
        val delimiter = ","
        val waitTimeMills = 1000L
        val queue = Queue<String>()
        val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, waitTimeMills, queue)
        assertNotNull(shipmentTracker)
    }

    @Test(expected = IllegalArgumentException::class)
    fun testInvalidConstruction() {
        val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
            Pair("shipped", ::Shipped),
            Pair("location", ::Location),
            // Missing "created" key intentionally
        )
        val delimiter = ","
        val waitTimeMills = 1000L
        val queue = Queue<String>()

        // This should throw an IllegalArgumentException
        val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, waitTimeMills, queue)
    }

    @Test
    fun testRun() {
        val queue = Queue<String>()
        val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
            Pair("created", ::Created)
        )
        val delimiter = ","
        val waitTimeMills = 100L

        val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, waitTimeMills, queue)

        runBlocking {
            val job = launch {
                shipmentTracker.run()
            }

            val updateInfo = "created,123,1626177123"
            queue.enqueue(updateInfo)

            delay(waitTimeMills * 2)

            val shipment = shipmentTracker.findShipment("123")
            assertNotNull(shipment)
            assertEquals("123", shipment?.id)

            job.cancelAndJoin()
        }
    }

    @Test
    fun testRun_EmptyUpdate() {
        val queue = Queue<String>()
        val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
            Pair("created", ::Created)
        )
        val delimiter = ","
        val waitTimeMills = 100L

        val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, waitTimeMills, queue)

        runBlocking {
            val job = launch {
                shipmentTracker.run()
            }

            // No update in the queue

            delay(waitTimeMills * 2)

            val shipment = shipmentTracker.findShipment("123")
            assertNull(shipment)

            job.cancelAndJoin()
        }
    }

    @Test
    fun testRun_UnknownUpdateType() {
        val queue = Queue<String>()
        val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
            Pair("created", ::Created)
        )
        val delimiter = ","
        val waitTimeMills = 100L

        val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, waitTimeMills, queue)

        runBlocking {
            val job = launch {
                shipmentTracker.run()
            }

            val unknownUpdate = "unknown,123,1626177123"
            queue.enqueue(unknownUpdate)

            delay(waitTimeMills * 2)

            val shipment = shipmentTracker.findShipment("123")
            assertNull(shipment)

            job.cancelAndJoin()
        }
    }

    @Test
    fun testRun_ErrorCreatingUpdate() {
        val queue = Queue<String>()
        val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
            Pair("created") { _, _, _, _ -> throw IllegalArgumentException("Invalid update") }
        )
        val delimiter = ","
        val waitTimeMills = 100L

        val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, waitTimeMills, queue)

        runBlocking {
            val job = launch {
                shipmentTracker.run()
            }

            val updateInfo = "created,123,1626177123"
            queue.enqueue(updateInfo)

            delay(waitTimeMills * 2)

            val shipment = shipmentTracker.findShipment("123")
            assertNull(shipment)

            job.cancelAndJoin()
        }
    }

    @Test
    fun testFindShipment() {
        val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
            Pair("created", ::Created)
        )
        val delimiter = ","
        val waitTimeMills = 1000L
        val queue = Queue<String>()
        val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, waitTimeMills, queue)

        val shipmentId = "123"
        val shipment = Shipment(shipmentId)
        shipmentTracker.addShipment(shipment)

        val foundShipment = shipmentTracker.findShipment(shipmentId)

        assertNotNull(foundShipment)
        assertEquals(shipmentId, foundShipment?.id)
    }


    @Test
    fun testAddShipment() {
        val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
            Pair("created", ::Created)
        )
        val delimiter = ","
        val waitTimeMills = 1000L
        val queue = Queue<String>()
        val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, waitTimeMills, queue)

        val shipmentId = "123"
        val shipment = Shipment(shipmentId)
        shipmentTracker.addShipment(shipment)

        val foundShipment = shipmentTracker.findShipment(shipmentId)

        assertNotNull(foundShipment)
        assertEquals(shipmentId, foundShipment?.id)
    }


    @Test
    fun testGetUpdate() {
        val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
            Pair("created", ::Created),
            Pair("shipped", ::Shipped),
        )
        val delimiter = ","
        val waitTimeMills = 1000L
        val queue = Queue<String>()
        val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, waitTimeMills, queue)

        val info1 = "created,123,1626177123"
        val update1 = shipmentTracker.getUpdate(info1)

        assertNotNull(update1)
        assertEquals("created", update1?.type)
        assertEquals("123", update1?.shipmentId)

        val info2 = "other,123,1626177123"
        val update2 = shipmentTracker.getUpdate(info2)
        assertNull(update2)

        val info3 = "shipped,123,1626177123"
        val update3 = shipmentTracker.getUpdate(info3)
        assertNull(update3)

        val info4 = "shipped,123"
        val update4 = shipmentTracker.getUpdate(info4)
        assertNull(update4)
    }

}