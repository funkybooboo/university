package listener

import junit.framework.TestCase.*
import kotlin.test.Test

class QueueTest {
    @Test
    fun testConstruction() {
        val queue = Queue<String>()
        assertNotNull(queue)
    }

    @Test
    fun testEnqueueAndDequeue() {
        val item = "TestItem"
        val queue = Queue<String>()
        queue.enqueue(item)
        val value = queue.dequeue()
        assertEquals(value, item)
    }
}