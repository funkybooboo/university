package subject.shipment

import subject.ShippingUpdate
import subject.update.Update

class ExpressShipment(
    id: String,
    notes: MutableList<String> = mutableListOf(),
    updateHistory: MutableList<ShippingUpdate> = mutableListOf(),
    expectedDeliveryDateTimestampHistory: MutableList<Long> = mutableListOf(),
    locationHistory: MutableList<String> = mutableListOf(),
    abnormalOccurrenceHistory: MutableList<String> = mutableListOf()
) : Shipment(
    id,
    notes,
    updateHistory,
    expectedDeliveryDateTimestampHistory,
    locationHistory,
    abnormalOccurrenceHistory
) {
    override fun copy(): Shipment {
        return ExpressShipment(
            id,
            notes.toMutableList(),
            updateHistory.toMutableList(),
            expectedDeliveryDateTimestampHistory.toMutableList(),
            locationHistory.toMutableList(),
            abnormalOccurrenceHistory.toMutableList()
        )
    }

    override fun validate(update: Update) {
        val createdTimestamp = updateHistory.firstOrNull()?.timestamp ?: return
        val expectedDeliveryTimestamp = update.timestampOfUpdate
        if (expectedDeliveryTimestamp - createdTimestamp > 3 * 24 * 60 * 60 * 1000) {
            abnormalOccurrenceHistory.add("Expected delivery date exceeds allowed limit for Express shipment")
        }
    }
}
