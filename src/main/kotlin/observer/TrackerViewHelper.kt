package observer

import androidx.compose.runtime.mutableStateMapOf
import logger.Logger.Level
import subject.Shipment
import manager.LoggerManager.logger

class TrackerViewHelper() : ShipmentObserver {

    val shipments = mutableStateMapOf<String, Shipment>()

    override fun notify(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Notification received for shipment: ${shipment.id}")
        shipments[shipment.id] = shipment
        println("hello1")
    }

    fun startTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Starting tracking for shipment: ${shipment.id}")
        shipment.addObserver(this)
        shipments[shipment.id] = shipment
    }

    fun stopTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Stopping tracking for shipment: ${shipment.id}")
        shipments[shipment.id]?.removeObserver(this)
        shipments.remove(shipment.id)
    }
}
