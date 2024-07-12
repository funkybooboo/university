package listener

import java.util.LinkedList
import logger.Logger.Level
import logger

class Queue<T>() {
    private val items: LinkedList<T> = LinkedList()

    fun enqueue(item: T) {
        items.add(item)
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Enqueued item: $item")
    }

    fun dequeue(): T? {
        val item = if (items.isNotEmpty()) {
            items.removeFirst()
        } else {
            null
        }
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Dequeued item: $item")
        return item
    }
}
