package manager

import tracker.TrackerServer

object TrackerServerManager {
    private const val port = 3000
    val trackerServer = TrackerServer(port)
}