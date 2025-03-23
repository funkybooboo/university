import unittest

import ex2

class ExerciseTests(unittest.TestCase):
    def test_validityOfLists(self):
        '''
        Tests the validity of the output with a list as input.
            EX: Ensures listOfASCIIInts(['A', 'B', 'C']) == [65, 66, 67]
        '''
        # Test a small list, one of the examples in the README
        input = ['A', 'B', 'C']
        expOutput = [65, 66, 67]
        self.assertEquals(ex2.listOfASCIIInts(input), expOutput)

        # Test a sentence with lower and uppercase letters
        input = ['A', ' ', 's', 'h', 'o', 'r', 't', ' ', 's', 'e', 'n', 't', 'e', 'n', 'c', 'e', '.']
        expOutput = [65, 32, 115, 104, 111, 114, 116, 32, 115, 101, 110, 116, 101, 110, 99, 101, 46]
        self.assertEquals(ex2.listOfASCIIInts(input), expOutput)

        # Test the start and end of printable ascii chars in first 128 chars
        input = [' ', '~']
        expOutput = [32, 126]
        self.assertEquals(ex2.listOfASCIIInts(input), expOutput)

        # One more, for good luck
        input = ["A", "B", "c", "1", "-", "_", "~", " ", "z", "Y", "x"]
        expOutput = [65, 66, 99, 49, 45, 95, 126, 32, 122, 89, 120]
        self.assertEquals(ex2.listOfASCIIInts(input), expOutput)

    def test_validityOfStrings(self):
        '''
        Tests the validity of the output with a string as input.
            EX: Ensures listOfASCIIInts('ABC') == [65, 66, 67]
        '''
        # Test a small list, one of the examples in the README
        input = 'ABC'
        expOutput = [65, 66, 67]
        self.assertEquals(ex2.listOfASCIIInts(input), expOutput)

        # Test a sentence with lower and uppercase letters
        input = 'A short sentence.'
        expOutput = [65, 32, 115, 104, 111, 114, 116, 32, 115, 101, 110, 116, 101, 110, 99, 101, 46]
        self.assertEquals(ex2.listOfASCIIInts(input), expOutput)

        # Test the start and end of printable ascii chars in first 128 chars
        input = ' ~'
        expOutput = [32, 126]
        self.assertEquals(ex2.listOfASCIIInts(input), expOutput)

        # Another one, just to push it to it's limits
        input = "Will you pass this test too?"
        expOutput = [87, 105, 108, 108, 32, 121, 111, 117, 32, 112, 97, 115, 115, 32, 116, 104, 105, 115, 32, 116, 101, 115, 116, 32, 116, 111, 111, 63]
        self.assertEquals(ex2.listOfASCIIInts(input), expOutput)

    def test_emptyList(self):
        '''
        Tests that the function returns an empty list if an empty list is given.
        '''
        input = []
        expOutput = []
        self.assertEquals(ex2.listOfASCIIInts(input), expOutput)
