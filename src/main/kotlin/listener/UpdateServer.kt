package listener

import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.http.HttpStatusCode
import io.ktor.server.application.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import kotlinx.coroutines.channels.Channel
import java.io.File
import logger.Logger.Level
import manager.LoggerManager.logger

class UpdateServer(channel: Channel<String>, private val port: Int): UpdateListener(channel) {
    override suspend fun listen() {
        logger.log(Level.INFO, "UpdateServer", "Starting server on port $port")

        embeddedServer(Netty, port = port) {
            routing {
                get("/") {
                    try {
                        logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Shipment Updater page request")
                        val file = File("/home/nate/Software/School/CS5700_Program3/src/main/resources/static/update.html")
                        if (file.exists()) {
                            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Shipment Updater page request successful")
                            call.respondFile(file)
                        } else {
                            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Shipment Updater page request failed")
                            call.respond(HttpStatusCode.NotFound, "File not found")
                        }
                    } catch (e: Exception) {
                        logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Failed to handle GET request: ${e.message}")
                        call.respond(HttpStatusCode.InternalServerError, "Failed to handle request")
                    }
                }

                post("/update") {
                    try {
                        val text = call.receiveText()
                        if (text.isBlank()) {
                            call.respond(HttpStatusCode.BadRequest, "Empty or blank update not allowed")
                        } else {
                            val lines = text.split('\n')
                            for (line in lines) {
                                channel.send(line)
                            }
                            call.respondText("Received and queued: $text")
                        }
                    } catch (e: Exception) {
                        logger.log(Level.ERROR, Thread.currentThread().threadId().toString(), "Failed to handle POST request: ${e.message}")
                        call.respond(HttpStatusCode.InternalServerError, "Failed to handle request")
                    }
                }
            }
        }.start()

        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Update Server started on port $port")
    }
}
