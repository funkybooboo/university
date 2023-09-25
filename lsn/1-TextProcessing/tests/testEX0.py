import io
import sys
import unittest

import ex0

class ExerciseTests(unittest.TestCase):

    def test_validity(self):
        '''
        Tests the validity of the output.
            EX: findWords(["keep reject"]) == ["keep"]
        '''
        input = "Keep Reject Keep Keep Reject"
        expOutput = ["Keep", "Keep", "Keep"]
        self.assertEquals(ex0.findWords(input), expOutput)

        input = "Is this the real life?"
        expOutput = ["Is", "this", "the", "real", "life?"]
        self.assertEquals(ex0.findWords(input), expOutput)

        input = "Or is this just fantasy"
        expOutput = ["Or", "is", "this", "just"]
        self.assertEquals(ex0.findWords(input), expOutput)


    def test_emptyList(self):
        '''
        Tests the validity of the output when an empty list is expected.
            EX: findWords("reject") == []
                findWords("") == []
        '''
        input = ""
        self.assertTrue(ex0.findWords(input) == [] or ex0.findWords(input) == [''])

        input = "Reject"
        expOutput = []
        self.assertEquals(ex0.findWords(input), expOutput)
