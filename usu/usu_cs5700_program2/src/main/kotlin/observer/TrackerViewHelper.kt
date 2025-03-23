package observer

import androidx.compose.runtime.mutableStateMapOf
import logger.Logger.Level
import subject.Shipment
import manager.LoggerManager.logger

class TrackerViewHelper() : ShipmentObserver {

    private val _shipments = mutableStateMapOf<String, Shipment>()
    val shipments: Map<String, Shipment>
        get() = _shipments

    override fun notify(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Notification received for shipment: ${shipment.id}")
        _shipments[shipment.id] = shipment
    }

    fun startTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Starting tracking for shipment: ${shipment.id}")
        shipment.addObserver(this)
        _shipments[shipment.id] = shipment
    }

    fun stopTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Stopping tracking for shipment: ${shipment.id}")
        _shipments[shipment.id]?.removeObserver(this)
        _shipments.remove(shipment.id)
    }
}
