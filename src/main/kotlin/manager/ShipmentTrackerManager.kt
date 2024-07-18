package manager

import ShipmentTracker
import manager.QueueManager.queue
import subject.update.*

object ShipmentTrackerManager {
    private val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update> = mapOf(
        Pair("created", ::Created),
        Pair("shipped", ::Shipped),
        Pair("location", ::Location),
        Pair("delivered", ::Delivered),
        Pair("delayed", ::Delayed),
        Pair("lost", ::Lost),
        Pair("canceled", ::Canceled),
        Pair("noteadded", ::NoteAdded),
    )
    private const val delimiter = ","
    val shipmentTracker = ShipmentTracker(typeToUpdateConstructor, delimiter, queue)
}