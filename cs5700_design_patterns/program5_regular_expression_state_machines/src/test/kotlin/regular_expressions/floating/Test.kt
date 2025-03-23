package regular_expressions.floating

import regular_expressions.FirstStateFactory
import regular_expressions.Verifier
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class Test {
    private val firstStateFactory = FirstStateFactory()

    @Test
    fun testValidFloats() {
        val verifier = Verifier()
        assertTrue(verifier.verify("1.0", firstStateFactory.createFirstState("float")!!))
        assertTrue(verifier.verify("123.34", firstStateFactory.createFirstState("float")!!))
        assertTrue(verifier.verify("0.20000", firstStateFactory.createFirstState("float")!!))
        assertTrue(verifier.verify("12349871234.12340981234098", firstStateFactory.createFirstState("float")!!))
        assertTrue(verifier.verify(".123", firstStateFactory.createFirstState("float")!!))
    }

    @Test
    fun testInvalidFloats() {
        val verifier = Verifier()
        assertFalse(verifier.verify("", firstStateFactory.createFirstState("float")!!))
        assertFalse(verifier.verify("123", firstStateFactory.createFirstState("float")!!))
        assertFalse(verifier.verify("123.123.", firstStateFactory.createFirstState("float")!!))
        assertFalse(verifier.verify("123.02a", firstStateFactory.createFirstState("float")!!))
        assertFalse(verifier.verify("123.", firstStateFactory.createFirstState("float")!!))
        assertFalse(verifier.verify("012.4", firstStateFactory.createFirstState("float")!!))
    }
}