package regular_expressions.password

import regular_expressions.FirstStateFactory
import regular_expressions.Verifier
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class Test {
    private val firstStateFactory = FirstStateFactory()

    @Test
    fun testValidPasswords() {
        val verifier = Verifier()
        assertTrue(verifier.verify("aaaaH!aa", firstStateFactory.createFirstState("password")!!))
        assertTrue(verifier.verify("1234567*9J", firstStateFactory.createFirstState("password")!!))
        assertTrue(verifier.verify("asdpoihj;loikjasdf;ijp;lij2309jasd;lfkm20ij@aH", firstStateFactory.createFirstState("password")!!))
    }

    @Test
    fun testInvalidPasswords() {
        val verifier = Verifier()
        assertFalse(verifier.verify("a", firstStateFactory.createFirstState("password")!!))
        assertFalse(verifier.verify("aaaaaaa!", firstStateFactory.createFirstState("password")!!))
        assertFalse(verifier.verify("aaaHaaaaa", firstStateFactory.createFirstState("password")!!))
        assertFalse(verifier.verify("Abbbbbbb!", firstStateFactory.createFirstState("password")!!))
    }
}