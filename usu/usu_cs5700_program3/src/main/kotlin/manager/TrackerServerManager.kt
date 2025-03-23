package manager

import observer.TrackerServer
import manager.TrackerViewHelperManager.trackerViewHelper

object TrackerServerManager {
    private const val port = 3000
    val trackerServer = TrackerServer(trackerViewHelper, port)
}