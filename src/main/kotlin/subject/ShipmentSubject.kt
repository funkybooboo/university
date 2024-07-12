package subject

import observer.ShipmentObserver
import logger.Logger.Level
import manager.LoggerManager.logger

abstract class ShipmentSubject() {
    protected var observers: MutableList<ShipmentObserver> = mutableListOf()

    abstract fun notifyObservers()

    open fun addObserver(observer: ShipmentObserver) {
        if (!observers.contains(observer)) {
            observers.add(observer)
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Observer added: $observer")
        } else {
            logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Observer $observer already exists, not added again")
        }
    }

    fun removeObserver(observer: ShipmentObserver) {
        observers = observers.filter { it != observer }.toMutableList()
        logger.log(Level.INFO, Thread.currentThread().threadId().toString(), "Observer removed: $observer")
    }
}
