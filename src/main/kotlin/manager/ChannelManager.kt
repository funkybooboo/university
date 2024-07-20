package manager

import kotlinx.coroutines.channels.Channel

object ChannelManager {
    val channel = Channel<String>()
}