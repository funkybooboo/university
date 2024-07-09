package subject.update

class Delivered(
    type: String,
    shipmentId: String,
    timeStampOfUpdate: Long,
    otherInfo: String?
): Update(type, shipmentId, timeStampOfUpdate, otherInfo) {
    override fun getLocation(): String? {
        return null
    }

    override fun getNote(): String? {
        return null
    }

    override fun getExpectedDeliveryDateTimestamp(): Long {
        return 0
    }
}