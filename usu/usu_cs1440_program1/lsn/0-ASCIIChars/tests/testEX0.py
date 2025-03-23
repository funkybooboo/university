import io
import sys
import unittest

import ex0

class ExerciseTests(unittest.TestCase):

    @classmethod
    def setUpClass(self):
        self.file = io.StringIO()
        sys.stdout = self.file
        sys.stderr = self.file

    @classmethod
    def tearDownClass(self):
        # I could do something with self.file here...
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__

    def test_success(self):
        ex0.displayASCII()
