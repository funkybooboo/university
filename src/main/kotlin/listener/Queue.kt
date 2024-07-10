package listener

import java.util.LinkedList

class Queue<T>() {
    private val elements: LinkedList<T> = LinkedList()

    fun enqueue(item: T) {
        elements.add(item)
    }

    fun dequeue(): T? {
        return if (elements.isNotEmpty()) {
            elements.removeFirst()
        } else {
            null
        }
    }
}
