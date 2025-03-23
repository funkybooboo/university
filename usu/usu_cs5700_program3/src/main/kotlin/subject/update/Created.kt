package subject.update

import logger.Logger.Level
import manager.LoggerManager.logger

class Created(
    updateType: String,
    shipmentType: String?,
    shipmentId: String,
    timeStampOfUpdate: Long,
    otherInfo: String?
) : Update(updateType, shipmentType, shipmentId, timeStampOfUpdate, otherInfo) {

    init {
        if (updateType != "created") throw IllegalArgumentException("Invalid type")
        if (otherInfo != null) throw IllegalArgumentException("otherInfo must be null")
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Created Created update for shipment: $shipmentId")
    }

    override fun getLocation(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting location for Created update (shipment: $shipmentId)")
        return null
    }

    override fun getNote(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting note for Created update (shipment: $shipmentId)")
        return null
    }

    override fun getExpectedDeliveryDateTimestamp(): Long? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting expected delivery date timestamp for Created update (shipment: $shipmentId)")
        return null
    }
}
