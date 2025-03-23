import unittest
import ex3

class ExerciseTests(unittest.TestCase):
    def test_validity_singleCharacter(self):
        '''
        This test ensures that the score of each single character is correct.
        Checks all alphabetic characters individually.
        '''

        # Check all uppercase letters
        base = ord('A')
        for i in range(26):
            self.assertEquals(ex3.scoreWord(chr(base + i)), i)

        # Check all lowercase letters
        base = ord('a')
        for i in range(26):
            self.assertEquals(ex3.scoreWord(chr(base + i)), i)

    def test_validity_lettersOnly(self):
        '''
        This test ensures that the score of each word is summed up correctly.
        This test ommits special characters, and only tests alphabetic
        characters.
            EX: wordScore("ABC") == 3
        '''

        input = "ABC"
        expOutput = 3
        self.assertEquals(ex3.scoreWord(input), expOutput)

        input = "CAT"
        expOutput = 21
        self.assertEquals(ex3.scoreWord(input), expOutput)

        input = "aAaA"
        expOutput = 0
        self.assertEquals(ex3.scoreWord(input), expOutput)

        input = "supercalifragilisticexpialidocious"
        expOutput = 345
        self.assertEquals(ex3.scoreWord(input), expOutput)

    def test_validity_mixedChars(self):
        '''
        This test ensures that the score of each word is summed up correctly.
        This test includes non-alphabetic characters, which should have a score
        of 0.
            EX: wordScore("A3BC") == 3
        '''

        input = "!#@%^"
        expOutput = 0
        self.assertEquals(ex3.scoreWord(input), expOutput)

        input = "Zap!"
        expOutput = 40
        self.assertEquals(ex3.scoreWord(input), expOutput)

        input = "What's this doing here?"
        expOutput = 194
        self.assertEquals(ex3.scoreWord(input), expOutput)

        input = "Tr!ck3d y4!"
        expOutput = 75
        self.assertEquals(ex3.scoreWord(input), expOutput)

    def test_caseInsensitive(self):
        '''
        This test does not care about the validity of the result. Rather, it
        ensures that the word score is case insensitive by checking that the
        results of the same word--in different cases--is the exact same.
            EX: scoreWord('Queen') == scoreWord('queEN'): Can still pass
                even if scoreWord('Queen') != 57, the correct score
        '''
        result1 = ex3.scoreWord('o')
        result2 = ex3.scoreWord('O')
        self.assertEquals(result1, result2)

        result1 = ex3.scoreWord('ONE')
        result2 = ex3.scoreWord('One')
        self.assertEquals(result1, result2)

        result1 = ex3.scoreWord('UPPERCASE')
        result2 = ex3.scoreWord('uppercase')
        self.assertEquals(result1, result2)

    def test_emptyString(self):
        '''
        Verifies that the empty string returns a score of 0.
        '''
        input = ''
        expOutput = 0
        self.assertEquals(ex3.scoreWord(input), expOutput)
