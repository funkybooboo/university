package subject.update

import logger.Logger.Level
import manager.LoggerManager.logger

class Canceled(
    type: String,
    shipmentId: String,
    timeStampOfUpdate: Long,
    otherInfo: String?
) : Update(type, shipmentId, timeStampOfUpdate, otherInfo) {

    init {
        if (type != "canceled") throw IllegalArgumentException("Invalid type")
        if (otherInfo != null) throw IllegalArgumentException("otherInfo must be null")
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Created Canceled update for shipment: $shipmentId")
    }

    override fun getLocation(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting location for Canceled update (shipment: $shipmentId)")
        return null
    }

    override fun getNote(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting note for Canceled update (shipment: $shipmentId)")
        return null
    }

    override fun getExpectedDeliveryDateTimestamp(): Long {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting expected delivery date timestamp for Canceled update (shipment: $shipmentId)")
        return -1
    }
}
