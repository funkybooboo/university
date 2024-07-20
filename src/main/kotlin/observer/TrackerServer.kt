package observer

import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.engine.*
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
            install(WebSockets)

            routing {
                get("/") {
                    logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Shipment Tracker page requested")
                    val file = File("/home/nate/Software/School/CS5700_Program3/src/main/resources/static/tracker.html")
                    if (file.exists()) {
                        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Shipment Tracker page request successful")
                        call.respondFile(file)
                    } else {
                        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Shipment Tracker page request failed")
                        call.respond(HttpStatusCode.NotFound, "File not found")
                    }
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
                                            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Client $clientId started tracking shipment $shipmentId")
                                        } else {
                                            logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Shipment $shipmentId not found for client $clientId")
                                            send("Shipment not found: $shipmentId")
                                        }
                                    }
                                    text.startsWith("STOP_TRACKING:") -> {
                                        val shipmentId = text.removePrefix("STOP_TRACKING:").trim()
                                        val shipment = shipmentTracker.findShipment(shipmentId)
                                        if (shipment != null) {
                                            trackerViewHelper.stopTracking(shipment)
                                            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Client $clientId stopped tracking shipment $shipmentId")
                                        } else {
                                            logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Shipment $shipmentId not found for client $clientId")
                                            send("Shipment not found: $shipmentId")
                                        }
                                    }
                                    else -> {
                                        logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Unsupported command from client $clientId: $text")
                                        send("Unsupported command: $text")
                                    }
                                }
                            }
                        }
                    } catch (e: ClosedReceiveChannelException) {
                        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Client $clientId disconnected")
                    } catch (e: Throwable) {
                        logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Error handling WebSocket connection for client $clientId: ${e.message}")
                    } finally {
                        connections.remove(this)
                        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Connection closed for client $clientId")
                    }
                }
            }
        }.start()

        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Tracker Server started on port $port")
    }

    suspend fun broadcastUpdate(shipment: Shipment) {
        connections.keys.forEach { connection ->
            try {
                connection.send(shipment.toJson())
                logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Update broadcasted to client")
            } catch (e: Exception) {
                logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Failed to send update to client: ${e.message}")
            }
        }
    }
}
