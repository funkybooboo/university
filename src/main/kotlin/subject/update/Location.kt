package subject.update

class Location(
    type: String,
    shipmentId: String,
    timeStampOfUpdate: Long,
    otherInfo: String?
): Update(type, shipmentId, timeStampOfUpdate, otherInfo) {
    override fun getLocation(): String? {
        return otherInfo
    }

    override fun getNote(): String? {
        return null
    }

    override fun getExpectedDeliveryDateTimestamp(): Long? {
        return null
    }
}