package subject.shipment

import subject.ShippingUpdate
import subject.update.Update
import java.text.SimpleDateFormat
import java.util.Date

class BulkShipment(
    id: String,
    shipmentType: String,
    notes: MutableList<String> = mutableListOf(),
    updateHistory: MutableList<ShippingUpdate> = mutableListOf(),
    expectedDeliveryDateTimestampHistory: MutableList<Long> = mutableListOf(),
    locationHistory: MutableList<String> = mutableListOf(),
    abnormalOccurrenceHistory: MutableList<String> = mutableListOf()
) : Shipment(
    id,
    shipmentType,
    notes,
    updateHistory,
    expectedDeliveryDateTimestampHistory,
    locationHistory,
    abnormalOccurrenceHistory
) {
    override fun copy(): Shipment {
        return BulkShipment(
            id,
            shipmentType,
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
        val minimumAllowedTimestamp = createdTimestamp + 3 * 24 * 60 * 60 * 1000

        if (expectedDeliveryTimestamp < minimumAllowedTimestamp) {
            val sdf = SimpleDateFormat("yyyy-MM-dd")
            val minimumAllowedDate = sdf.format(Date(minimumAllowedTimestamp))
            val expectedDeliveryDate = sdf.format(Date(expectedDeliveryTimestamp))
            abnormalOccurrenceHistory.add("Expected delivery date of $shipmentType shipment is too soon. Should be at least $minimumAllowedDate, but got $expectedDeliveryDate.")
        }
    }
}
