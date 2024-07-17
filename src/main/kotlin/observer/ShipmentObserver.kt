package observer

import subject.Shipment

interface ShipmentObserver {
    suspend fun notify(shipment: Shipment)
}