import unittest

import ex2

class ExerciseTests(unittest.TestCase):
    def test_validity(self):
        '''
        Tests the validity of the output with a list as input.
            EX: Ensures cleanSentence("What #do you expect?") == ["What", "you", "expect?"]
        '''
        # Test a small list, one of the examples in the README
        input = "What do you expect?"
        expOutput = ["What", "do", "you", "expect?"]
        self.assertEquals(ex2.cleanSentence(input), expOutput)

        input = "What #do you expect?"
        expOutput = ["What", "you", "expect?"]
        self.assertEquals(ex2.cleanSentence(input), expOutput)

        # Test a small list, one of the examples in the README
        input = "#What do #you #expect?"
        expOutput = ["do"]
        self.assertEquals(ex2.cleanSentence(input), expOutput)


    def test_emptyList(self):
        '''
        Tests that the function returns an empty list when expected.
            EX: cleanSentence("#Nothing") == []
        '''
        input = ""
        expOutput = []
        self.assertEquals(ex2.cleanSentence(input), expOutput)

        input = "#Nothing #should #be #here."
        expOutput = []
        self.assertEquals(ex2.cleanSentence(input), expOutput)
