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

        val parts = info.split(delimiter).map { it.trim() }
        if (parts.size > 4 || parts.size < 3) {
            logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "Invalid update: $info")
        }

        val updateType = parts[0]

        val updateConstructor = updateTypeToUpdateConstructor[updateType]
        if (updateConstructor == null) {
            logger.log(Level.WARNING, Thread.currentThread().threadId().toString(), "Unknown update type: $updateType")
            return null
        }
        try {
            if (updateType == "created") {
                val shipmentType = parts[1]
                val shipmentId = parts[2]
                val timestampOfUpdate = parts[3].toLong()
                return updateConstructor(updateType, shipmentType, shipmentId, timestampOfUpdate, null)
            }
            else {
                val shipmentId = parts[1]
                val timestampOfUpdate = parts[2].toLong()
                val otherInfo = parts.getOrNull(3)
                return updateConstructor(updateType, null, shipmentId, timestampOfUpdate, otherInfo)
            }
        }
        catch (e: Exception) {
            logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Invalid Update Error: ${e.message}")
            return null
        }
    }
}
