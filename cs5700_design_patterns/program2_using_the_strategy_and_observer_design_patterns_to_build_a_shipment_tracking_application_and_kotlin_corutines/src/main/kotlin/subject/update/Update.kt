package subject.update

abstract class Update(
    val type: String,
    val shipmentId: String,
    val timestampOfUpdate: Long,
    val otherInfo: String?,
) {
    abstract fun getLocation(): String?

    abstract fun getNote(): String?

    abstract fun getExpectedDeliveryDateTimestamp(): Long?
}