package subject.update

import logger.Logger.Level
import manager.LoggerManager.logger

class Shipped(
    updateType: String,
    shipmentType: String?,
    shipmentId: String,
    timeStampOfUpdate: Long,
    otherInfo: String?
) : Update(updateType, shipmentType, shipmentId, timeStampOfUpdate, otherInfo) {

    init {
        if (updateType != "shipped") throw IllegalArgumentException("Invalid type")
        if (otherInfo == null) throw IllegalArgumentException("otherInfo must have time information")
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Created Shipped update for shipment: $shipmentId")
    }

    override fun getLocation(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting location for Shipped update (shipment: $shipmentId)")
        return null
    }

    override fun getNote(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting note for Shipped update (shipment: $shipmentId)")
        return null
    }

    override fun getExpectedDeliveryDateTimestamp(): Long {
        val timestamp = otherInfo?.toLong()
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting expected delivery date timestamp for Shipped update (shipment: $shipmentId): $timestamp")
        return timestamp!!
    }
}
