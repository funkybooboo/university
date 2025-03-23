import unittest
import unittest.mock
import sys
import io

import ex0

def concatenate(file):
    '''
    Prints the contents of a text file line by line, from the beginning. Similar to the `cat` command in the shell. File is an opened file with read permissions. This tool does *not* close a file when it is finished.
    '''
    file.seek(0)
    for line in file.readlines():
        print(line, end='')

class ExerciseTests(unittest.TestCase):
    def test_validity(self):
        '''
        Tests whether or not the outputted file of exercise 0 is correct when
        given a valid path.
        '''

        input = "data/text1"
        expOutputFile = open(input)

        expOutput = io.StringIO()
        sys.stdout = expOutput
        concatenate(expOutputFile)
        sys.stdout = sys.__stdout__

        result = io.StringIO()
        sys.stdout = result
        ex0.printContentsOfFile(input)
        sys.stdout = sys.__stdout__

        self.assertEquals(result.getvalue().strip(), expOutput.getvalue().strip())
