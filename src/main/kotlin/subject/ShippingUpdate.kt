package subject

import java.text.SimpleDateFormat
import java.util.*
import logger.Level
import logger

class ShippingUpdate(
    private val previousStatus: String,
    val newStatus: String,
    private val timestamp: Long
) {
    private val dateFormat = SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

    override fun toString(): String {
        val formattedTime = dateFormat.format(Date(timestamp))
        val logMessage = "Shipment went from $previousStatus to $newStatus at $formattedTime"
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), logMessage)
        return logMessage
    }
}
