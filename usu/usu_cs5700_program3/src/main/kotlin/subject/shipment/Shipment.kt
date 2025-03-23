package subject.shipment

import subject.update.Update
import logger.Logger.Level
import manager.LoggerManager.logger
import observer.ShipmentObserver
import subject.ShipmentSubject
import subject.ShippingUpdate

abstract class Shipment(
    val id: String,
    val shipmentType: String,
    val notes: MutableList<String> = mutableListOf(),
    val updateHistory: MutableList<ShippingUpdate> = mutableListOf(),
    val expectedDeliveryDateTimestampHistory: MutableList<Long> = mutableListOf(),
    val locationHistory: MutableList<String> = mutableListOf(),
    val abnormalOccurrenceHistory: MutableList<String> = mutableListOf()
): ShipmentSubject() {

    override suspend fun notifyObservers() {
        observers.forEach {
            it.notify(copy())
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Notifying observer for shipment: $id")
        }
    }

    override suspend fun addObserver(observer: ShipmentObserver) {
        super.addObserver(observer)
        observer.notify(copy())
    }

    override fun hasObserver(observer: ShipmentObserver): Boolean {
        return observers.contains(observer)
    }

    fun addUpdate(update: Update) {
        addNote(update.getNote())
        addLocation(update.getLocation())
        addExpectedDeliveryDateTimestamp(update.getExpectedDeliveryDateTimestamp())

        val previousState = if (updateHistory.isEmpty()) "none" else updateHistory.last().newStatus

        val shippingUpdate = ShippingUpdate(update.updateType, previousState, update.timestampOfUpdate)
        updateHistory.add(shippingUpdate)

        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Update added for shipment: $id, Type: ${update.updateType}")
    }

    private fun addNote(note: String?) {
        if (note != null) {
            notes.add(note)
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Note added for shipment: $id - Note: $note")
        }
    }

    private fun addLocation(location: String?) {
        if (location != null) {
            locationHistory.add(location)
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Location added for shipment: $id - Location: $location")
        }
    }

    private fun addExpectedDeliveryDateTimestamp(expectedDeliveryDateTimestamp: Long?) {
        if (expectedDeliveryDateTimestamp != null) {
            expectedDeliveryDateTimestampHistory.add(expectedDeliveryDateTimestamp)
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Expected delivery date timestamp added for shipment: $id - Timestamp: $expectedDeliveryDateTimestamp")
        }
    }

    fun toJson(): String {
        val updatesJson = updateHistory.joinToString(",\n") { update ->
            """{
                "newStatus": "${update.newStatus}",
                "previousStatus": "${update.previousStatus}",
                "timestamp": "${update.timestamp}"
            }""".trimIndent()
        }

        val notesJson = notes.joinToString(",\n") { note ->
            """{
                "note": "$note"
            }""".trimIndent()
        }

        val abnormalOccurrencesJson = abnormalOccurrenceHistory.joinToString(",\n") { occurrence ->
            """{
                "abnormalOccurrence": "$occurrence"
            }""".trimIndent()
        }

        return """
            {
                "id": "$id",
                "shipmentType": "$shipmentType",
                "location": "${locationHistory.lastOrNull() ?: "None"}",
                "expectedDelivery": "${expectedDeliveryDateTimestampHistory.lastOrNull()?.let { java.util.Date(it).toString() } ?: "None"}",
                "updateHistory": [
                    $updatesJson
                ],
                "notes": [
                    $notesJson
                ],
                "abnormalOccurrences": [
                    $abnormalOccurrencesJson
                ]
            }
        """.trimIndent()
    }

    abstract fun validate(update: Update)

    abstract fun copy(): Shipment
}
