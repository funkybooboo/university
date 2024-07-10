package observer

import androidx.compose.runtime.mutableStateMapOf
import subject.Shipment
import logger.Level
import logger

class TrackerViewHelper(): ShipmentObserver {

    val shipments = mutableStateMapOf<String, Shipment>()

    override fun notify(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Notification received for shipment: ${shipment.id}")
        shipments[shipment.id] = shipment
    }

    fun startTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Starting tracking for shipment: ${shipment.id}")
        shipment.let {
            it.addObserver(this)
            shipments[shipment.id] = it
        }
    }

    fun stopTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Stopping tracking for shipment: ${shipment.id}")
        shipments[shipment.id]?.removeObserver(this)
        shipments.remove(shipment.id)
    }
}
