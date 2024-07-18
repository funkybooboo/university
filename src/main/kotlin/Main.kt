import kotlinx.coroutines.*
import logger.Logger.Level
import manager.UpdateListenerManager.updateServer
import manager.LoggerManager.logger
import manager.ShipmentTrackerManager.shipmentTracker
import manager.TrackerServerManager.trackerServer

fun main(): Unit = runBlocking {
    val shipmentTrackerJob = launch {
        try {
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Start shipment tracker")
            shipmentTracker.listen()
        } catch (e: Throwable) {
            logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Shipment tracker encountered an error: ${e.message}")
            throw e
        }
    }

    val updateServerJob = launch {
        try {
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Start update server")
            updateServer.listen()
        } catch (e: Throwable) {
            logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Update server encountered an error: ${e.message}")
            throw e
        }
    }

    val trackerServerJob = launch {
        try {
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Start tracker server")
            trackerServer.listen()
        } catch (e: Throwable) {
            logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Tracker server encountered an error: ${e.message}")
            throw e
        }
    }

    // Wait for all jobs to complete
    shipmentTrackerJob.join()
    updateServerJob.join()
    trackerServerJob.join()

    // Check for cancellations
    if (shipmentTrackerJob.isCancelled || updateServerJob.isCancelled || trackerServerJob.isCancelled) {
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Closing the application due to error in one of the components")
    }
}
