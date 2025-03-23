package manager

import TrackingSimulator
import manager.QueueManager.queue
import subject.update.*

object TrackingSimulatorManager {
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
    private const val waitTimeMills = 1000L
    val trackingSimulator = TrackingSimulator(typeToUpdateConstructor, delimiter, waitTimeMills, queue)
}