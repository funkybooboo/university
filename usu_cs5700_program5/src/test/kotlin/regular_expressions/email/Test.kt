package regular_expressions.email

import regular_expressions.FirstStateFactory
import regular_expressions.Verifier
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class Test {
    private val firstStateFactory = FirstStateFactory()

    @Test
    fun testValidEmails() {
        val verifier = Verifier()
        assertTrue(verifier.verify("a@b.c", firstStateFactory.createFirstState("email")!!))
        assertTrue(verifier.verify("joseph.ditton@usu.edu", firstStateFactory.createFirstState("email")!!))
        assertTrue(verifier.verify("{}*\$.&\$*(@*\$%&.*&*", firstStateFactory.createFirstState("email")!!))
    }

    @Test
    fun testInvalidEmails() {
        val verifier = Verifier()
        assertFalse(verifier.verify("@b.c", firstStateFactory.createFirstState("email")!!))
        assertFalse(verifier.verify("a@b@c.com", firstStateFactory.createFirstState("email")!!))
        assertFalse(verifier.verify("a.b@b.b.c", firstStateFactory.createFirstState("email")!!))
        assertFalse(verifier.verify("joseph ditton@usu.edu", firstStateFactory.createFirstState("email")!!))
    }
}