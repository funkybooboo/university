import listener.FileReader
import listener.Queue
import subject.Shipment
import subject.update.Update

class TrackingSimulator(
    private val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update>,
    private val fileName: String,
    private val delimiter: String,
    private val waitTime: Long
) {
    private val shipments: MutableList<Shipment> = mutableListOf()

    private fun findShipment(id: String): Shipment? {
        return shipments.find { it.id == id }
    }

    private fun addShipment(shipment: Shipment) {
        shipments.add(shipment)
    }

    fun run() {
        val queue = Queue<String>()
        val fileReader = FileReader(queue, fileName)

        fileReader.listen()

        while (true) {
            Thread.sleep(waitTime)

            val info = queue.dequeue()?.strip() ?: continue
            if (info == "") continue

            // TODO notify user that an invalid update occurred
            val update = getUpdate(info) ?: continue

            if (update.type == "created") {
                addShipment(Shipment(update.shipmentId))
            }

            val shipment = findShipment(update.shipmentId)
            if (shipment == null) {
                // TODO notify user that an update occurred on a nonexistent shipment
            } else {
                shipment.addUpdate(update)
                shipment.notifyObservers()
            }
        }
    }

    private fun getUpdate(info: String): Update? {
        val parts = info.split(delimiter).map { it.lowercase() }
        if (parts.size < 3 || parts.size > 4) return null
        val type = parts[0]
        val shipmentId = parts[1]
        val timestampOfUpdate = parts[2].toLong()
        val otherInfo = parts[3]
        val updateConstructor = typeToUpdateConstructor[type] ?: return null

        return try {
            updateConstructor(type, shipmentId, timestampOfUpdate, otherInfo)
        } catch (e: Exception) {
            e.printStackTrace()
            null
        }
    }
}
