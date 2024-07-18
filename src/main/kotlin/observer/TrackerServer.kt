package observer

import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.http.content.*
import io.ktor.server.netty.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.websocket.*
import io.ktor.websocket.*
import kotlinx.coroutines.channels.ClosedReceiveChannelException
import kotlinx.coroutines.channels.consumeEach
import manager.LoggerManager.logger
import logger.Logger.Level
import manager.ShipmentTrackerManager.shipmentTracker
import subject.Shipment
import java.io.File
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

class TrackerServer(private val trackerViewHelper: TrackerViewHelper, private val port: Int) {
    private val connections = ConcurrentHashMap<DefaultWebSocketSession, AtomicInteger>()

    suspend fun listen() {
        embeddedServer(Netty, port = port) {
            routing {

                get("/") {
                    logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Tracker page request")
                    val resourceUrl = call.resolveResource("tracker.html")
                    val file = File(resourceUrl.toString())
                    call.respondFile(file)
                }

                webSocket("/track-shipments") {
                    val clientId = connections.computeIfAbsent(this) { AtomicInteger() }.getAndIncrement()

                    logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "New client connected: $clientId")

                    try {
                        // Send initial data to the client
                        trackerViewHelper.shipments.values.forEach { shipment ->
                            send(shipment.toJson())
                        }

                        // Listen for incoming messages from the client
                        incoming.consumeEach { frame ->
                            if (frame is Frame.Text) {
                                val text = frame.readText().trim()

                                // Handle commands from the client
                                when {
                                    text.startsWith("START_TRACKING:") -> {
                                        val shipmentId = text.removePrefix("START_TRACKING:").trim()
                                        val shipment = shipmentTracker.findShipment(shipmentId)
                                        if (shipment != null) {
                                            trackerViewHelper.startTracking(shipment)
                                            broadcastUpdate(shipment)
                                        }
                                    }
                                    text.startsWith("STOP_TRACKING:") -> {
                                        val shipmentId = text.removePrefix("STOP_TRACKING:").trim()
                                        val shipment = shipmentTracker.findShipment(shipmentId)
                                        if (shipment != null) {
                                            trackerViewHelper.stopTracking(shipment)
                                            broadcastUpdate(shipment)
                                        }
                                    }
                                    else -> {
                                        logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Unsupported command from client: $text")
                                        send("Unsupported command: $text")
                                    }
                                }
                            }
                        }
                    } catch (e: ClosedReceiveChannelException) {
                        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Client disconnected: $clientId")
                    } catch (e: Throwable) {
                        logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Error handling WebSocket connection: ${e.message}")
                    } finally {
                        connections.remove(this)
                    }
                }
            }
        }.start(wait = true)
    }

    suspend fun broadcastUpdate(shipment: Shipment) {
        connections.keys.forEach { connection ->
            try {
                connection.send(shipment.toJson())
            } catch (e: Exception) {
                logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Failed to send update to client: ${e.message}")
            }
        }
    }
}
