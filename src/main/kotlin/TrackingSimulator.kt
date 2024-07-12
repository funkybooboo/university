import kotlinx.coroutines.delay
import listener.Queue
import subject.Shipment
import subject.update.Update
import logger.Logger.Level

class TrackingSimulator(
    private val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update>,
    private val delimiter: String,
    private val waitTimeMills: Long,
    private val queue: Queue<String>,
) {
    private val shipments: MutableList<Shipment> = mutableListOf()

    suspend fun run() {
        while (true) {
            delay(waitTimeMills)

            val info = queue.dequeue()?.trim()
            if (info.isNullOrBlank()) {
                logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "No updates")
                continue
            }

            val update = getUpdate(info)
            if (update == null) {
                logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "An unknown update occurred: $info")
                continue
            }

            if (update.type == "created") {
                logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Creating new shipment: ${update.shipmentId}")
                addShipment(Shipment(update.shipmentId))
            }

            val shipment = findShipment(update.shipmentId)
            if (shipment == null) {
                logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "Update occurred on non-existing shipment: ${update.shipmentId}")
                continue
            }

            shipment.addUpdate(update)
            shipment.notifyObservers()

            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Update processed for shipment: ${update.shipmentId}, Type: ${update.type}")
        }
    }

    fun findShipment(id: String): Shipment? {
        val foundShipment = shipments.find { it.id == id }
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Finding shipment: $id - Found: ${foundShipment != null}")
        return foundShipment?.copy()
    }

    private fun addShipment(shipment: Shipment) {
        shipments.add(shipment)
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Added new shipment: ${shipment.id}")
    }

    private fun getUpdate(info: String): Update? {
        val parts = info.split(delimiter).map { it.lowercase() }
        if (parts.size < 3 || parts.size > 4) {
            logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "Invalid update format: $info")
            return null
        }

        val type = parts[0]
        val shipmentId = parts[1]
        val timestampOfUpdate = parts[2].toLong()
        val otherInfo = parts.getOrNull(3)
        val updateConstructor = typeToUpdateConstructor[type]

        return if (updateConstructor == null) {
            logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "Unknown update type: $type")
            null
        } else {
            try {
                updateConstructor(type, shipmentId, timestampOfUpdate, otherInfo)
            } catch (e: Exception) {
                logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Error creating update: ${e.message}")
                e.printStackTrace()
                null
            }
        }
    }
}
