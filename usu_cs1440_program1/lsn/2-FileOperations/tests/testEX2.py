import unittest
import sys
import io

import ex2


def print1(file):
    content = file.read()
    print(content, end='')


def print2(file):
    lines = file.readlines()
    for line in lines:
        print(line, end='')


class ExerciseTests(unittest.TestCase):
    def test_validity_printTwice(self):
        '''
        Tests whether or not the outputted text of exercise 2 `printTwice`
        function is correct when given a valid path.
        '''

        path = 'data/text1'
        file = open(path)

        expOutputStream = io.StringIO()
        sys.stdout = expOutputStream
        print1(file)
        file.seek(0)
        print2(file)
        sys.stdout = sys.__stdout__

        resultStream = io.StringIO()
        sys.stdout = resultStream
        ex2.printTwice(path)
        sys.stdout = sys.__stdout__

        expOutput = expOutputStream.getvalue().strip()
        result = resultStream.getvalue().strip()
        self.assertEquals(expOutput, result)

    def test_validity_printContents1(self):
        '''
        Tests whether or not the outputted text of exercise 2's `printContents1`
        function is correct when given a valid path.
        '''

        path = 'data/text1'
        file = open(path)

        expOutputStream = io.StringIO()
        sys.stdout = expOutputStream
        print1(file)
        sys.stdout = sys.__stdout__

        resultStream = io.StringIO()
        sys.stdout = resultStream
        file.seek(0)
        ex2.printContents1(file)
        sys.stdout = sys.__stdout__

        expOutput = expOutputStream.getvalue().strip()
        result = resultStream.getvalue().strip()
        self.assertEquals(expOutput, result)

    def test_validity_printContents2(self):
        '''
        Tests whether or not the outputted text of exercise 2's `printContents2`
        function is correct when given a valid path.
        '''

        path = 'data/text1'
        file = open(path)

        expOutputStream = io.StringIO()
        sys.stdout = expOutputStream
        print2(file)
        sys.stdout = sys.__stdout__

        resultStream = io.StringIO()
        sys.stdout = resultStream
        file.seek(0)
        ex2.printContents2(file)
        sys.stdout = sys.__stdout__

        expOutput = expOutputStream.getvalue().strip()
        result = resultStream.getvalue().strip()
        self.assertEquals(expOutput, result)

    def test_invalid(self):
        '''
        Tests the output when an invalid file is requested.
        '''

        input = "data/FileDNE"
        # Taken from https://stackoverflow.com/questions/15672151/is-it-possible-for-a-unit-test-to-assert-that-a-method-calls-sys-exit
        sys.stdout = io.StringIO()
        with self.assertRaises(SystemExit):
            ex2.printTwice(input)
        sys.stdout = sys.__stdout__
