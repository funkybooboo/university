package subject.shipment

import subject.ShippingUpdate
import subject.update.Update
import java.text.SimpleDateFormat
import java.util.Date

class OvernightShipment(
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
        return OvernightShipment(
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
        val minimumAllowedTimestamp = createdTimestamp + 24 * 60 * 60 * 1000
        val maximumAllowedTimestamp = createdTimestamp + 2 * 24 * 60 * 60 * 1000

        if (expectedDeliveryTimestamp < minimumAllowedTimestamp || expectedDeliveryTimestamp > maximumAllowedTimestamp) {
            val sdf = SimpleDateFormat("yyyy-MM-dd")
            val minAllowedDate = sdf.format(Date(minimumAllowedTimestamp))
            val maxAllowedDate = sdf.format(Date(maximumAllowedTimestamp))
            val expectedDeliveryDate = sdf.format(Date(expectedDeliveryTimestamp))
            abnormalOccurrenceHistory.add("Expected delivery date of $shipmentType shipment is not within the allowed limits. Should be between $minAllowedDate and $maxAllowedDate, but got $expectedDeliveryDate.")
        }
    }
}
