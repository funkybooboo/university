import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.delay
import listener.FileReader
import listener.Queue
import subject.Shipment
import subject.update.Update

class TrackingSimulator(
    private val typeToUpdateConstructor: Map<String, (String, String, Long, String?) -> Update>,
    private val fileName: String,
    private val delimiter: String,
    private val waitTimeMills: Long
) {
    private val shipments: MutableList<Shipment> = mutableListOf()

    suspend fun run() {
        val queue = Queue<String>()

        val fileReader = FileReader(queue, fileName)
        fileReader.listen()

        coroutineScope {
            while (true) {
                delay(waitTimeMills)

                val info = queue.dequeue()?.strip()
                if (info == "" || info == null) {
                    println("No updates")
                    continue
                }

                val update = getUpdate(info)

                if (update == null) {
                    println("An unknown update occurred")
                    continue
                }

                if (update.type == "created") {
                    addShipment(Shipment(update.shipmentId))
                }

                val shipment = findShipment(update.shipmentId)
                if (shipment == null) {
                    println("Update occurred on non existing shipment")
                    continue
                }

                shipment.addUpdate(update)
                shipment.notifyObservers()
            }
        }
    }

    fun findShipment(id: String): Shipment? {
        return shipments.find { it.id == id }
    }

    private fun addShipment(shipment: Shipment) {
        shipments.add(shipment)
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
