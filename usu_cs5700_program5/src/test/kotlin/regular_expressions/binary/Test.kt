package regular_expressions.binary

import regular_expressions.FirstStateFactory
import regular_expressions.Verifier
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class Test {
    private val firstStateFactory = FirstStateFactory()

    @Test
    fun testValidBinaries() {
        val verifier = Verifier()
        assertTrue(verifier.verify("1", firstStateFactory.createFirstState("binary")!!))
        assertTrue(verifier.verify("11", firstStateFactory.createFirstState("binary")!!))
        assertTrue(verifier.verify("101", firstStateFactory.createFirstState("binary")!!))
        assertTrue(verifier.verify("111111", firstStateFactory.createFirstState("binary")!!))
        assertTrue(verifier.verify("10011010001", firstStateFactory.createFirstState("binary")!!))
    }

    @Test
    fun testInvalidBinaries() {
        val verifier = Verifier()
        assertFalse(verifier.verify("01", firstStateFactory.createFirstState("binary")!!))
        assertFalse(verifier.verify("10", firstStateFactory.createFirstState("binary")!!))
        assertFalse(verifier.verify("1000010", firstStateFactory.createFirstState("binary")!!))
        assertFalse(verifier.verify("100a01", firstStateFactory.createFirstState("binary")!!))
    }
}