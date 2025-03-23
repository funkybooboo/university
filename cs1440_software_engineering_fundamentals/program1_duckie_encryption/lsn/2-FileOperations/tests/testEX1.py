import unittest
import sys
import io

import ex1

class ExerciseTests(unittest.TestCase):
    def test_validity(self):
        '''
        Tests whether or not the outputted file of exercise 1 is correct when
        given a valid path.
        '''

        input = "data/text0.txt"
        expOutput = open(input)
        sys.stdout = io.StringIO()
        result = ex1.getFileSafely(input)
        sys.stdout = sys.__stdout__
        self.assertEquals(result.name, expOutput.name)
        self.assertEquals(type(result), type(expOutput))


    def test_invalid(self):
        '''
        Tests the output when an invalid file is requested.
        '''

        input = "data/FileDNE"
        # Taken from https://stackoverflow.com/questions/15672151/is-it-possible-for-a-unit-test-to-assert-that-a-method-calls-sys-exit
        sys.stdout = io.StringIO()
        with self.assertRaises(SystemExit):
            ex1.getFileSafely(input)
        sys.stdout = sys.__stdout__
