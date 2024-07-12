package subject.update

import logger.Logger.Level
import logger

class Lost(
    type: String,
    shipmentId: String,
    timeStampOfUpdate: Long,
    otherInfo: String?
) : Update(type, shipmentId, timeStampOfUpdate, otherInfo) {

    init {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Created Lost update for shipment: $shipmentId")
    }

    override fun getLocation(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting location for Lost update (shipment: $shipmentId)")
        return ""
    }

    override fun getNote(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting note for Lost update (shipment: $shipmentId)")
        return null
    }

    override fun getExpectedDeliveryDateTimestamp(): Long {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting expected delivery date timestamp for Lost update (shipment: $shipmentId)")
        return -1
    }
}
