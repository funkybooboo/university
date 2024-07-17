package listener

import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.application.*
import io.ktor.http.HttpStatusCode

class UpdateServer(queue: Queue<String>, private val port: Int): UpdateListener(queue) {
    override suspend fun listen() {
        embeddedServer(Netty, port = port) {
            routing {
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
