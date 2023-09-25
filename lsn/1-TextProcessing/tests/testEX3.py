import unittest
import ex3

class ExerciseTests(unittest.TestCase):
    def test_validity_both(self):
        '''
        Verifies that both lists returned contain the correct output for the given input.
            EX: cleanSentenceTwoLists("Clean #Dirty") == (["Clean"], ["Dirty"])
        '''
        input = "Clean #Dirty"
        expOutput = (["Clean"], ["Dirty"])
        output = ex3.cleanSentenceTwoLists(input)
        self.assertEquals(output[0], expOutput[0])
        self.assertEquals(output[1], expOutput[1])

    def test_validity_clean(self):
        '''
        Verifies that the clean list returned contains the correct output for the given input.
            EX: cleanSentenceTwoLists("Clean #Dirty")[0] == ["Clean"]
        '''
        input = "Clean #Dirty"
        expOutput = ["Clean"]
        self.assertEquals(ex3.cleanSentenceTwoLists(input)[0], expOutput)

    def test_validity_dirty(self):
        '''
        Verifies that the dirty list returned contains the correct output for the given input.
            EX: cleanSentenceTwoLists("Clean #Dirty")[1] == ["Dirty"]
        '''
        input = "Clean #Dirty"
        expOutput = ["Dirty"]
        self.assertEquals(ex3.cleanSentenceTwoLists(input)[1], expOutput)

    def test_removalOfCharFromDirty(self):
        '''
        Verifies that the special character is removed from the contents of the `dirty` list.
            EX: cleanSentenceTwoLists("#Dirty")[1] == ["Dirty"]
        '''
        input = "#Dirty"
        expOutput = ["Dirty"]
        self.assertEquals(ex3.cleanSentenceTwoLists(input)[1], expOutput)

    def test_emptyList_both(self):
        '''
        Verifies that an empty list is returned for both the `clean` and `dirty` list when expected.
            EX: cleanSentenceTwoLists("") == ([], [])
        '''
        input = ""
        expOutput = ([], [])
        output = ex3.cleanSentenceTwoLists(input)
        self.assertEquals(output[0], expOutput[0])
        self.assertEquals(output[1], expOutput[1])


    def test_emptyList_clean(self):
        '''
        Verifies that an empty list is returned for the `clean` list when expected.
            EX: cleanSentenceTwoLists("#Only #Dirty")[0] == []
        '''
        input = "#Only #Dirty"
        expOutput = []
        self.assertEquals(ex3.cleanSentenceTwoLists(input)[0], expOutput)

    def test_emptyList_dirty(self):
        '''
        Verifies that an empty list is returned for the `dirty` list when expected.
            EX: cleanSentenceTwoLists("Only clean")[1] == []
        '''
        input = "Only clean"
        expOutput = []
        self.assertEquals(ex3.cleanSentenceTwoLists(input)[1], expOutput)
