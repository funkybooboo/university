package tracker

import manager.TrackerViewHelperManager.trackerViewHelper
import manager.ShipmentTrackerManager.shipmentTracker
import manager.LoggerManager.logger
import logger.Logger.Level

import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.routing.*
import io.ktor.server.websocket.*
import io.ktor.websocket.*
import subject.Shipment
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

class TrackerServer(private val port: Int) {
    private val connections = ConcurrentHashMap<DefaultWebSocketSession, AtomicInteger>()

    suspend fun listen() {
        embeddedServer(Netty, port = port) {
            routing {
                webSocket("/track-shipments") {
                    val clientId = connections.computeIfAbsent(this) { AtomicInteger() }.getAndIncrement()

                    logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "New client connected: $clientId")

                    try {
                        // Send initial data
                        for (shipment in trackerViewHelper.shipments.values) {
                            send(shipment.toJson())
                        }

                        // Listen for incoming messages
                        for (frame in incoming) {
                            frame as? Frame.Text ?: continue
                            val text = frame.readText()
                            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Received from client: $text")

                            // Handle commands from the client
                            when {
                                text.startsWith("START_TRACKING:") -> {
                                    val shipmentId = text.removePrefix("START_TRACKING:").trim()
                                    val shipment = shipmentTracker.findShipment(shipmentId)
                                    if (shipment != null) {
                                        trackerViewHelper.startTracking(shipment)
                                    }
                                }
                                text.startsWith("STOP_TRACKING:") -> {
                                    val shipmentId = text.removePrefix("STOP_TRACKING:").trim()
                                    val shipment = shipmentTracker.findShipment(shipmentId)
                                    if (shipment != null) {
                                        trackerViewHelper.stopTracking(shipment)
                                    }
                                }
                                else -> {
                                    logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Unsupported command from client: $text")
                                    send("Unsupported command: $text")
                                }
                            }
                        }
                    } catch (e: Exception) {
                        logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Error in WebSocket connection: ${e.message}")
                    } finally {
                        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Client disconnected: $clientId")
                        connections.remove(this)
                    }
                }
            }
        }.start(wait = true)
    }

    suspend fun broadcastUpdate(shipment: Shipment) {
        for (connection in connections.keys) {
            try {
                connection.send(shipment.toJson())
            } catch (e: Exception) {
                logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Failed to send update to client: ${e.message}")
            }
        }
    }
}
