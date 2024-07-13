package subject

import java.text.SimpleDateFormat
import java.util.*
import logger.Logger.Level
import manager.LoggerManager.logger

class ShippingUpdate(
    val newStatus: String,
    val previousStatus: String,
    val timestamp: Long
) {
    private val dateFormat = SimpleDateFormat("yyyy-MM-dd HH:mm")

    override fun toString(): String {
        val formattedTime = dateFormat.format(Date(timestamp))
        val message = "Shipment went from ${previousStatus.ifEmpty { "none" }} to $newStatus at $formattedTime"
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), message)
        return message
    }
}
