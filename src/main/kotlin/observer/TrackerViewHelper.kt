package observer

import androidx.compose.runtime.mutableStateMapOf
import subject.Shipment
import logger.Level
import logger

class TrackerViewHelper(): ShipmentObserver {

    private val _shipments = mutableStateMapOf<String, Shipment>()

    var shipments: List<Shipment> = _shipments.toList().map { pair -> pair.second }
        private set

    override fun notify(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Notification received for shipment: ${shipment.id}")
        _shipments[shipment.id] = shipment
    }

    fun startTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Starting tracking for shipment: ${shipment.id}")
        shipment.let {
            it.addObserver(this)
            _shipments[shipment.id] = it
        }
    }

    fun stopTracking(shipment: Shipment) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Stopping tracking for shipment: ${shipment.id}")
        _shipments[shipment.id]?.removeObserver(this)
        _shipments.remove(shipment.id)
    }
}
