package subject.update

import logger.Logger.Level
import manager.LoggerManager.logger

class NoteAdded(
    updateType: String,
    shipmentType: String?,
    shipmentId: String,
    timeStampOfUpdate: Long,
    otherInfo: String?
) : Update(updateType, shipmentType, shipmentId, timeStampOfUpdate, otherInfo) {

    init {
        if (updateType != "noteadded") throw IllegalArgumentException("Invalid type")
        if (otherInfo == null) throw IllegalArgumentException("otherInfo must have note information")
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Created NoteAdded update for shipment: $shipmentId")
    }

    override fun getLocation(): String? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting location for NoteAdded update (shipment: $shipmentId)")
        return null
    }

    override fun getNote(): String? {
        val note = otherInfo
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting note for NoteAdded update (shipment: $shipmentId): $note")
        return note
    }

    override fun getExpectedDeliveryDateTimestamp(): Long? {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Getting expected delivery date timestamp for NoteAdded update (shipment: $shipmentId)")
        return null
    }
}
