package manager

import ShipmentTracker
import manager.ChannelManager.channel
import manager.UpdateFactoryManager.updateFactory
import manager.ShipmentFactoryManager.shipmentFactory

object ShipmentTrackerManager {
    val shipmentTracker = ShipmentTracker(channel, updateFactory, shipmentFactory)
}