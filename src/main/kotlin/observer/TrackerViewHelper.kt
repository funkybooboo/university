package observer

import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import subject.Shipment
import logger.Level
import logger

class TrackerViewHelper() : ShipmentObserver {

    private val _shipments = mutableMapOf<String, Shipment>()

    var shipments: List<Shipment> by mutableStateOf(_shipments.values.toList())
        private set

    override fun notify(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Notification received for shipment: ${shipment.id}")
        _shipments[shipment.id] = shipment
        shipments = _shipments.values.toList()
    }

    fun startTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Starting tracking for shipment: ${shipment.id}")
        shipment.addObserver(this)
        _shipments[shipment.id] = shipment
        shipments = _shipments.values.toList()
    }

    fun stopTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Stopping tracking for shipment: ${shipment.id}")
        _shipments[shipment.id]?.removeObserver(this)
        _shipments.remove(shipment.id)
        shipments = _shipments.values.toList()
    }
}
