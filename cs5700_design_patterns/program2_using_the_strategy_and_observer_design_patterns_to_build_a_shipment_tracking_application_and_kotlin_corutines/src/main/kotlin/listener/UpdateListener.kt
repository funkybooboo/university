package listener

abstract class UpdateListener(
    protected val queue: Queue<String>
) {
    abstract suspend fun listen()
}