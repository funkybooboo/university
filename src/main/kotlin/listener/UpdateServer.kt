package listener

import io.ktor.server.application.*
import manager.QueueManager.queue

import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.response.*
import io.ktor.server.routing.*

class UpdateServer(private val port: Int): UpdateListener(queue) {
    override suspend fun listen() {
        embeddedServer(Netty, port = port) {
            routing {
                get("/") {
                    call.respondText("Hello, World!")
                }
            }
        }.start(wait = true)
    }
}
