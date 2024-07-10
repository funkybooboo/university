package listener

import java.util.LinkedList
import logger.Level
import logger

class Queue<T>() {
    private val elements: LinkedList<T> = LinkedList()

    fun enqueue(item: T) {
        elements.add(item)
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Enqueued item: $item")
    }

    fun dequeue(): T? {
        val item = if (elements.isNotEmpty()) {
            elements.removeFirst()
        } else {
            null
        }
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Dequeued item: $item")
        return item
    }
}
