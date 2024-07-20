package subject.update

import logger.Logger.Level
import manager.LoggerManager.logger

class UpdateFactory(
    private val updateTypeToUpdateConstructor: Map<String, (String, String?, String, Long, String?) -> Update>,
    private val delimiter: String,
) {

    init {
        if (!updateTypeToUpdateConstructor.keys.contains("created")) {
            throw IllegalArgumentException("updateTypeToUpdateConstructor must have 'created'")
        }
    }

    fun createUpdate(info: String): Update? {
        val parts = info.split(delimiter).map { it.trim().lowercase() }
        val updateType = parts[0]

        val updateConstructor = updateTypeToUpdateConstructor[updateType]
            ?: run {
                logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "Unknown update type: $updateType")
                return null
            }

        return try {
            val shipmentType: String? = if (updateType == "created") parts.getOrNull(1) else null
            val shipmentId: String = parts.getOrNull(2) ?: return null
            val timestampOfUpdate: Long = parts.getOrNull(3)?.toLongOrNull() ?: return null
            val otherInfo: String? = parts.getOrNull(4)

            updateConstructor(updateType, shipmentType, shipmentId, timestampOfUpdate, otherInfo)
        } catch (e: Exception) {
            logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Error creating update: ${e.message}")
            null
        }
    }
}
