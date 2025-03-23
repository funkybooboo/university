import unittest

import ex1

class ExerciseTests(unittest.TestCase):
    def test_validity(self):
        # Test a small list, one of the examples in the README
        input = [65, 66, 67]
        expOutput = ['A', 'B', 'C']
        self.assertEquals(ex1.listOfChars(input), expOutput)

        # Test a sentence with lower and uppercase letters
        input = [65, 32, 115, 104, 111, 114, 116, 32, 115, 101, 110, 116, 101, 110, 99, 101, 46]
        expOutput = ['A', ' ', 's', 'h', 'o', 'r', 't', ' ', 's', 'e', 'n', 't', 'e', 'n', 'c', 'e', '.']
        self.assertEquals(ex1.listOfChars(input), expOutput)

        # Test the start and end of printable ascii chars in first 128 chars
        input = [32, 126]
        expOutput = [' ', '~']
        self.assertEquals(ex1.listOfChars(input), expOutput)

    def test_emptyList(self):
        self.assertEquals(ex1.listOfChars([]), [])
