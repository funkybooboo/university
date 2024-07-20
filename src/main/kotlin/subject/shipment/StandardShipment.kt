package subject.shipment

import subject.ShippingUpdate
import subject.update.Update

class StandardShipment(
    id: String,
    notes: MutableList<String> = mutableListOf(),
    updateHistory: MutableList<ShippingUpdate> = mutableListOf(),
    expectedDeliveryDateTimestampHistory: MutableList<Long> = mutableListOf(),
    locationHistory: MutableList<String> = mutableListOf(),
    abnormalOccurrenceHistory: MutableList<String> = mutableListOf()
): Shipment(
    id,
    notes,
    updateHistory,
    expectedDeliveryDateTimestampHistory,
    locationHistory,
    abnormalOccurrenceHistory
) {
    override fun copy(): Shipment {
        return StandardShipment(
            id,
            notes.toMutableList(),
            updateHistory.toMutableList(),
            expectedDeliveryDateTimestampHistory.toMutableList(),
            locationHistory.toMutableList(),
            abnormalOccurrenceHistory.toMutableList()
        )
    }

    override fun validate(update: Update) {}
}