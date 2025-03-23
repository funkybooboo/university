package observer

import subject.Shipment

interface ShipmentObserver {
    fun notify(shipment: Shipment)
}