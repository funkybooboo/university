package subject.shipment

import subject.ShippingUpdate
import subject.update.Update
import java.text.SimpleDateFormat
import java.util.Date

class ExpressShipment(
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
        return ExpressShipment(
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
        if (!(update.updateType == "shipped" || update.updateType == "delayed")) return
        val createdTimestamp = updateHistory.firstOrNull()!!.timestamp
        val expectedDeliveryTimestamp = update.getExpectedDeliveryDateTimestamp()!!
        val maximumAllowedTimestamp = createdTimestamp + 3 * 24 * 60 * 60 * 1000

        if (expectedDeliveryTimestamp > maximumAllowedTimestamp) {
            val sdf = SimpleDateFormat("yyyy-MM-dd")
            val createdDate = sdf.format(Date(createdTimestamp))
            val expectedDeliveryDate = sdf.format(Date(expectedDeliveryTimestamp))
            abnormalOccurrenceHistory.add("Expected delivery date of $shipmentType shipment exceeds the allowed limit. Maximum allowed delivery within 3 days ($createdDate + 3 days), but got $expectedDeliveryDate.")
        }
    }
}
