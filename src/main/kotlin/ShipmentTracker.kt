import kotlinx.coroutines.channels.Channel
import subject.shipment.Shipment
import manager.LoggerManager.logger
import logger.Logger.Level
import subject.shipment.ShipmentFactory
import subject.update.UpdateFactory

class ShipmentTracker(
    private val channel: Channel<String>,
    private val updateFactory: UpdateFactory,
    private val shipmentFactory: ShipmentFactory,
) {
    private val shipments: MutableList<Shipment> = mutableListOf()

    suspend fun listen() {
        logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "Shipment Tracker has started listening")

        while (true) {
            val info = channel.receive().trim()
            if (info.isBlank()) {
                //logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "No updates")
                continue
            }

            val update = updateFactory.createUpdate(info)
            if (update == null) {
                logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "An unknown update occurred: $info")
                continue
            }

            if (update.updateType == "created") {
                logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Creating new shipment: ${update.shipmentId}")
                val shipment = shipmentFactory.createShipment(update.shipmentId, update.shipmentType!!)
                if (shipment == null) {
                    logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "Failed to create shipment: ${update.shipmentId}")
                    continue
                }
                addShipment(shipment)
            }

            val shipment = findShipment(update.shipmentId)
            if (shipment == null) {
                logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "Update occurred on non-existing shipment: ${update.shipmentId}")
                continue
            }

            shipment.validate(update)
            shipment.addUpdate(update)
            shipment.notifyObservers()

            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Update processed for shipment: ${update.shipmentId}, Type: ${update.updateType}")
        }
    }

    fun findShipment(id: String): Shipment? {
        val foundShipment = shipments.find { it.id == id }
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Finding shipment: $id - Found: ${foundShipment != null}")
        return foundShipment
    }

    private fun addShipment(shipment: Shipment) {
        shipments.add(shipment)
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Added new shipment: ${shipment.id}")
    }
}
