package regular_expressions.integer

import regular_expressions.FirstStateFactory
import regular_expressions.Verifier
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class Test {
    private val firstStateFactory = FirstStateFactory()

    @Test
    fun testValidIntegers() {
        val verifier = Verifier()
        assertTrue(verifier.verify("1", firstStateFactory.createFirstState("integer")!!))
        assertTrue(verifier.verify("123", firstStateFactory.createFirstState("integer")!!))
        assertTrue(verifier.verify("3452342352434534524346", firstStateFactory.createFirstState("integer")!!))
    }

    @Test
    fun testInvalidIntegers() {
        val verifier = Verifier()
        assertFalse(verifier.verify("", firstStateFactory.createFirstState("integer")!!))
        assertFalse(verifier.verify("0123", firstStateFactory.createFirstState("integer")!!))
        assertFalse(verifier.verify("132a", firstStateFactory.createFirstState("integer")!!))
        assertFalse(verifier.verify("0", firstStateFactory.createFirstState("integer")!!))
    }
}