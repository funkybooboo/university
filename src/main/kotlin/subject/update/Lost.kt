package subject.update

class Lost(
    type: String,
    shipmentId: String,
    timeStampOfUpdate: Long,
    otherInfo: String?
): Update(type, shipmentId, timeStampOfUpdate, otherInfo) {
    override fun getLocation(): String? {
        return ""
    }

    override fun getNote(): String? {
        return null
    }

    override fun getExpectedDeliveryDateTimestamp(): Long {
        return -1
    }
}