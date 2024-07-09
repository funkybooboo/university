package observer

import subject.Subject

interface Observer {
    fun update(subject: Subject)
}