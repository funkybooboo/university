package observer

import androidx.compose.runtime.mutableStateMapOf
import subject.Shipment
import subject.Subject

class TrackerViewHelper(): Observer {

    private val shipments = mutableStateMapOf<String, Shipment>()

    override fun update(subject: Subject) {
        if (subject is Shipment) {
            shipments[subject.id] = subject
        } else {
            // Handle unexpected subjects gracefully
            println("Received unexpected subject: $subject")
        }
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
