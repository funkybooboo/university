package subject.update

import logger.Logger.Level
import manager.LoggerManager.logger

class Delivered(
    type: String,
    shipmentId: String,
    timeStampOfUpdate: Long,
    otherInfo: String?
) : Update(type, shipmentId, timeStampOfUpdate, otherInfo) {

    init {
        if (type != "delivered") throw IllegalArgumentException("Invalid type")
        if (otherInfo != null) throw IllegalArgumentException("otherInfo must be null")
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Created Delivered update for shipment: $shipmentId")
    }

    override fun getLocation(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting location for Delivered update (shipment: $shipmentId)")
        return null
    }

    override fun getNote(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting note for Delivered update (shipment: $shipmentId)")
        return null
    }

    override fun getExpectedDeliveryDateTimestamp(): Long {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting expected delivery date timestamp for Delivered update (shipment: $shipmentId)")
        return 0
    }
}
