package subject.update

abstract class Update(
    val updateType: String,
    val shipmentType: String?,
    val shipmentId: String,
    val timestampOfUpdate: Long,
    val otherInfo: String?,
) {
    abstract fun getLocation(): String?

    abstract fun getNote(): String?

    abstract fun getExpectedDeliveryDateTimestamp(): Long?
}