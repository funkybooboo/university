package observer

import androidx.compose.runtime.mutableStateMapOf
import subject.Shipment

class TrackerViewHelper(): ShipmentObserver {

    val shipments = mutableStateMapOf<String, Shipment>()

    override fun notify(shipment: Shipment) {
        shipments[shipment.id] = shipment
    }

    fun startTracking(shipment: Shipment) {
        shipment.let {
            it.addObserver(this)
            shipments[shipment.id] = it
        }
    }

    fun stopTracking(shipment: Shipment) {
        shipments[shipment.id]?.removeObserver(this)
        shipments.remove(shipment.id)
    }
}
