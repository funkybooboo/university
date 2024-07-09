package subject.update

class Delayed(
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

    override fun getExpectedDeliveryDateTimestamp(): Long? {
        return otherInfo?.toLong()
    }
}