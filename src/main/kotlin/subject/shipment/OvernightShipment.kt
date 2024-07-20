package subject.shipment

import subject.ShippingUpdate
import subject.update.Update

class OvernightShipment(
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
        return OvernightShipment(
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
        val minimumAllowedTimestamp = createdTimestamp + 24 * 60 * 60 * 1000 // 1 day
        val maximumAllowedTimestamp = createdTimestamp + 2 * 24 * 60 * 60 * 1000 // 2 days
        if (expectedDeliveryTimestamp < minimumAllowedTimestamp || expectedDeliveryTimestamp > maximumAllowedTimestamp) {
            abnormalOccurrenceHistory.add("Expected delivery date is not within allowed limits for Overnight shipment")
        }
    }
}
