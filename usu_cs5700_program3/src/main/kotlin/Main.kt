import kotlinx.coroutines.*
import manager.ShipmentTrackerManager.shipmentTracker
import manager.TrackerServerManager.trackerServer
import manager.UpdateListenerManager.updateServer

fun main(): Unit = runBlocking {
    launch {
        shipmentTracker.listen()
    }

    launch {
        updateServer.listen()
    }

    launch {
        trackerServer.listen()
    }
}
