package subject.shipment

import subject.ShippingUpdate

class ShipmentFactory(
    private val shipmentTypeToShipmentConstructor: Map<String, (String, String, MutableList<String>, MutableList<ShippingUpdate>, MutableList<Long>, MutableList<String>, MutableList<String>) -> Shipment>,
) {
    fun createShipment(shipmentId: String, shipmentType: String, notes: MutableList<String> = mutableListOf(), updateHistory: MutableList<ShippingUpdate> = mutableListOf(), expectedDeliveryDateTimestampHistory: MutableList<Long> = mutableListOf(), locationHistory: MutableList<String> = mutableListOf(), abnormalOccurrenceHistory: MutableList<String> = mutableListOf()): Shipment? {
        val shipmentConstructor = shipmentTypeToShipmentConstructor[shipmentType]
        if (shipmentConstructor != null) {
            return shipmentConstructor(shipmentId, shipmentType, notes, updateHistory, expectedDeliveryDateTimestampHistory, locationHistory, abnormalOccurrenceHistory)
        }
        return null
    }
}
