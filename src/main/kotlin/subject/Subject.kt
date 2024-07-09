package subject

import observer.Observer

abstract class Subject {
    private var observers: MutableList<Observer> = mutableListOf()

    fun addObserver(observer: Observer) {
        observers.add(observer)
    }

    fun removeObserver(observer: Observer) {
        observers = observers.filter { it != observer }.toMutableList()
    }

    fun notifyObservers() {
        observers.forEach{it.update(this)}
    }
}