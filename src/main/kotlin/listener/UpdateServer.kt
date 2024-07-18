package listener

import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.http.HttpStatusCode
import io.ktor.server.application.*
import io.ktor.server.http.content.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import logger.Logger.Level
import java.io.File
import manager.LoggerManager.logger

class UpdateServer(private val queue: Queue<String>, private val port: Int) {
    suspend fun listen() {
        embeddedServer(Netty, port = port) {
            routing {
                get("/") {
                    logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Update page request")
                    val resourceUrl = call.resolveResource("update.html")
                    val file = File(resourceUrl.toString())
                    call.respondFile(file)
                }

                post("/update") {
                    val update = call.receiveText()
                    if (update.isBlank()) {
                        call.respond(HttpStatusCode.BadRequest, "Empty or blank update not allowed")
                    } else {
                        queue.enqueue(update)
                        call.respondText("Received and queued: $update")
                    }
                }
            }
        }.start(wait = true)
    }
}
