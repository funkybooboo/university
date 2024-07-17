import kotlinx.coroutines.*
import logger.Logger.Level
import manager.UpdateListenerManager.updateServer
import manager.LoggerManager.logger
import manager.ShipmentTrackerManager.shipmentTracker
import manager.TrackerServerManager.trackerServer

fun main(): Unit = runBlocking {
    launch {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Start shipment tracker")
        shipmentTracker.listen()
    }
    launch {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Start update server")
        updateServer.listen()
    }
    launch {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Start tracker server")
        trackerServer.listen()
    }
}
