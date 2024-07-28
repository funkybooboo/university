package emulator.computer.instruction

import com.natestott.emulator.computer.instruction.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*

class InstructionFactoryTest {

    private lateinit var instructionFactory: InstructionFactory

    @BeforeEach
    fun setUp() {
        instructionFactory = InstructionFactory()
    }

    @Test
    fun testCreateInstruction_Store() {
        val nibble0: Byte = 0x00
        val nibble1: Byte = 0x01
        val nibble2: Byte = 0x02
        val nibble3: Byte = 0x03

        val instruction = instructionFactory.createInstruction(nibble0, nibble1, nibble2, nibble3)

        assertTrue(instruction is Store)
        assertEquals("Store", instruction.javaClass.simpleName)
    }

    @Test
    fun testCreateInstruction_Add() {
        val nibble0: Byte = 0x01
        val nibble1: Byte = 0x01
        val nibble2: Byte = 0x02
        val nibble3: Byte = 0x03

        val instruction = instructionFactory.createInstruction(nibble0, nibble1, nibble2, nibble3)

        assertTrue(instruction is Add)
        assertEquals("Add", instruction.javaClass.simpleName)
    }

    @Test
    fun testCreateInstruction_Draw() {
        val nibble0: Byte = 0x0F
        val nibble1: Byte = 0x01
        val nibble2: Byte = 0x02
        val nibble3: Byte = 0x03

        val instruction = instructionFactory.createInstruction(nibble0, nibble1, nibble2, nibble3)

        assertTrue(instruction is Draw)
        assertEquals("Draw", instruction.javaClass.simpleName)
    }
}
