package manager

import listener.UpdateServer
import manager.ChannelManager.channel

object UpdateListenerManager {
    private const val port = 3001
    val updateServer = UpdateServer(channel, port)
}