package subject

import observer.ShipmentObserver

abstract class ShipmentSubject() {
    protected var observers: MutableList<ShipmentObserver> = mutableListOf()

    abstract fun notifyObservers()

    fun addObserver(observer: ShipmentObserver) {
        observers.add(observer)
    }

    fun removeObserver(observer: ShipmentObserver) {
        observers = observers.filter { it != observer }.toMutableList()
    }
}