import unittest

import ex1

class ExerciseTests(unittest.TestCase):
    def test_validity(self):
        '''
        Tests whether or not the output of exercise 1 is correct. Extra white
        space at the start or end of a string will not affect the tests.
        '''

        input = "This your implementation work has will a be small commended. issue."
        expOutput = "your work will be commended."
        self.assertEquals(ex1.everyOtherWord(input).strip(), expOutput)

        input = "whether do or or not do you not, succeed there is is up no to try. you."
        expOutput = "do or do not, there is no try."
        self.assertEquals(ex1.everyOtherWord(input).strip(), expOutput)

        # Test the start and end of printable ascii chars in first 128 chars
        input = "1 2 3 4 5 6 7 8"
        expOutput = "2 4 6 8"
        self.assertEquals(ex1.everyOtherWord(input).strip(), expOutput)

    def test_emptyStringResult(self):
        '''
        Tests if a given sentence that should return an empty string does
        return an empty string.
        '''

        input = ""
        expOutput = ""
        self.assertEquals(ex1.everyOtherWord(input).strip(), expOutput)

        input = "OneWord"
        expOutput = ""
        self.assertEquals(ex1.everyOtherWord(input).strip(), expOutput)
