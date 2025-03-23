package observer

import subject.shipment.Shipment

interface ShipmentObserver {
    suspend fun notify(shipment: Shipment)
}