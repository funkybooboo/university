package subject

import observer.ShipmentObserver
import logger.Level
import logger

abstract class ShipmentSubject() {
    protected var observers: MutableList<ShipmentObserver> = mutableListOf()

    abstract fun notifyObservers()

    fun addObserver(observer: ShipmentObserver) {
        observers.add(observer)
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Observer added: $observer")
    }

    fun removeObserver(observer: ShipmentObserver) {
        observers = observers.filter { it != observer }.toMutableList()
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Observer removed: $observer")
    }
}
