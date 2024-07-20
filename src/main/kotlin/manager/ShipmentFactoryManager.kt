package manager

import subject.ShippingUpdate
import subject.shipment.*

object ShipmentFactoryManager {
    private val shipmentTypeToShipmentConstructor:  Map<String, (String, MutableList<String>, MutableList<ShippingUpdate>, MutableList<Long>, MutableList<String>, MutableList<String>) -> Shipment> = mapOf(
        Pair("express", ::ExpressShipment),
        Pair("overnight", ::OvernightShipment),
        Pair("bulk", ::BulkShipment),
        Pair("standard", ::StandardShipment)
    )
    val shipmentFactory = ShipmentFactory(shipmentTypeToShipmentConstructor)
}