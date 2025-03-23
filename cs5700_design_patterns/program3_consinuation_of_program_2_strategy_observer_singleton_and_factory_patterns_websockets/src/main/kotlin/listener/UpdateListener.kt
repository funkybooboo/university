package listener

import kotlinx.coroutines.channels.Channel

abstract class UpdateListener(
    protected val channel: Channel<String>
) {
    abstract suspend fun listen()
}