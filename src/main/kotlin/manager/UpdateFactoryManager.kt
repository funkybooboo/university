package manager

import subject.update.*

object UpdateFactoryManager {
    private val updateTypeToUpdateConstructor: Map<String, (String, String?, String, Long, String?) -> Update> = mapOf(
        Pair("created", ::Created),
        Pair("shipped", ::Shipped),
        Pair("location", ::Location),
        Pair("delivered", ::Delivered),
        Pair("delayed", ::Delayed),
        Pair("lost", ::Lost),
        Pair("canceled", ::Canceled),
        Pair("noteadded", ::NoteAdded),
    )
    private const val delimiter = ","
    val updateFactory = UpdateFactory(updateTypeToUpdateConstructor, delimiter)

}