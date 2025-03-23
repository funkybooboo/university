#                         _  	    	       
#                        (o)<  DuckieCorp Software License  	    	       
#                   .____//  	    	       
#                    \ <' )   Copyright (c) 2022 Erik Falor  	    	       
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  	    	       
#  	    	       
# Permission is granted, to any person who is EITHER an employee OR  	    	       
# customer of DuckieCorp, to deal in the Software without restriction,  	    	       
# including without limitation the rights to use, copy, modify, merge,  	    	       
# publish, distribute, sublicense, and/or sell copies of the Software, and to  	    	       
# permit persons to whom the Software is furnished to do so, subject to the  	    	       
# following conditions:  	    	       
#  	    	       
# The above copyright notice and this permission notice shall be included in  	    	       
# all copies or substantial portions of the Software.  	    	       
#  	    	       
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  	    	       
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  	    	       
# FITNESS FOR A PARTICULAR PURPOSE, EDUCATIONAL VALUE AND NONINFRINGEMENT. IN  	    	       
# NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  	    	       
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR  	    	       
# OTHERWISE, ARISING FROM INDIGNATION, INDIGESTION, INDIFFERENCE, INDECENCY,  	    	       
# INDENTATION, INDETERMINATION, INTOXICATION, INDOCTRINATION, INTOLERANCE,  	    	       
# INDULGENCE, INDELICATENESS, INDISCRETION, INEFFECTIVENESS OR IN CONNECTION  	    	       
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.  	    	       

import unittest  	    	       

from Card import Card  	    	       
from RandNumberSet import RandNumberSet  	    	       


class TestCard(unittest.TestCase):  	    	       

    def setUp(self):  	    	       
        """  	    	       
        Create no fewer than 5 Card objects to test  	    	       

        Create a mixture of odd and even-sized cards  	    	       
        """
        ns1 = RandNumberSet(3, 100)
        ns2 = RandNumberSet(4, 150)
        ns3 = RandNumberSet(5, 200)
        ns4 = RandNumberSet(6, 250)
        ns5 = RandNumberSet(7, 300)
        ns6 = RandNumberSet(8, 350)

        self.card0 = Card(0, ns1)
        self.card1 = Card(1, ns2)
        self.card2 = Card(2, ns3)
        self.card3 = Card(3, ns4)
        self.card4 = Card(4, ns5)
        self.card5 = Card(5, ns6)

    def test_len(self):  	    	       
        """Assert that each card's size is as expected"""  	    	       
        self.assertEqual(len(self.card0), 3, "Ensure that card0 is sized as expected")
        self.assertEqual(len(self.card1), 4, "Ensure that card1 is sized as expected")
        self.assertEqual(len(self.card2), 5, "Ensure that card2 is sized as expected")
        self.assertEqual(len(self.card3), 6, "Ensure that card3 is sized as expected")
        self.assertEqual(len(self.card4), 7, "Ensure that card4 is sized as expected")
        self.assertEqual(len(self.card5), 8, "Ensure that card5 is sized as expected")

    def test_id(self):  	    	       
        """Assert that each card's ID number is as expected"""  	    	       
        self.assertEqual(self.card0.id(), 0, "Ensure that card0 is IDed as expected")
        self.assertEqual(self.card1.id(), 1, "Ensure that card1 is IDed as expected")
        self.assertEqual(self.card2.id(), 2, "Ensure that card2 is IDed as expected")
        self.assertEqual(self.card3.id(), 3, "Ensure that card3 is IDed as expected")
        self.assertEqual(self.card4.id(), 4, "Ensure that card4 is IDed as expected")
        self.assertEqual(self.card5.id(), 5, "Ensure that card5 is IDed as expected")

    def test_freeSquares(self):  	    	       
        """  	    	       
        Ensure that odd-sized cards have 1 "Free!" square in the center  	    	       
        Also test that even-sized cards do not have a "Free!" square by examining the 2x2 region about their centers  	    	       
        """

        self.assertEqual(self.card0.number_at(1, 1), "FREE!", "Ensure that there is a FREE! cell in the middle of card0")
        self.assertEqual(self.card2.number_at(2, 2), "FREE!", "Ensure that there is a FREE! cell in the middle of card2")
        self.assertEqual(self.card4.number_at(3, 3), "FREE!", "Ensure that there is a FREE! cell in the middle of card4")

        self.assertTrue(not self.card1.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card1")
        self.assertTrue(not self.card1.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card1")
        self.assertTrue(not self.card1.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card1")
        self.assertTrue(not self.card1.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card1")

        self.assertTrue(not self.card3.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card3")
        self.assertTrue(not self.card3.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card3")
        self.assertTrue(not self.card3.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card3")
        self.assertTrue(not self.card3.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card3")

        self.assertTrue(not self.card5.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card5")
        self.assertTrue(not self.card5.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card5")
        self.assertTrue(not self.card5.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card5")
        self.assertTrue(not self.card5.number_at(1, 1) == "FREE!", "Ensure that there is no FREE! cell in the middle of card5")

    def test_no_duplicates(self):  	    	       
        """Ensure that Cards do not contain duplicate numbers"""
        testList = list()
        testSet = set()
        for i in range(0, len(self.card0.ns)):
            for j in range(0, len(self.card0.ns)):
                testList.append(self.card0.card[i][j])
                testSet.add(self.card0.card[i][j])
        self.assertEqual(len(testList), len(testSet), "Ensure that there are no duplicate numbers in card0")
        testList.clear()
        testSet.clear()
        for i in range(0, len(self.card1.ns)):
            for j in range(0, len(self.card1.ns)):
                testList.append(self.card1.card[i][j])
                testSet.add(self.card1.card[i][j])
        self.assertEqual(len(testList), len(testSet), "Ensure that there are no duplicate numbers in card1")
        testList.clear()
        testSet.clear()
        for i in range(0, len(self.card2.ns)):
            for j in range(0, len(self.card2.ns)):
                testList.append(self.card2.card[i][j])
                testSet.add(self.card2.card[i][j])
        self.assertEqual(len(testList), len(testSet), "Ensure that there are no duplicate numbers in card2")
        testList.clear()
        testSet.clear()
        for i in range(0, len(self.card3.ns)):
            for j in range(0, len(self.card3.ns)):
                testList.append(self.card3.card[i][j])
                testSet.add(self.card3.card[i][j])
        self.assertEqual(len(testList), len(testSet), "Ensure that there are no duplicate numbers in card3")
        testList.clear()
        testSet.clear()
        for i in range(0, len(self.card4.ns)):
            for j in range(0, len(self.card4.ns)):
                testList.append(self.card4.card[i][j])
                testSet.add(self.card4.card[i][j])
        self.assertEqual(len(testList), len(testSet), "Ensure that there are no duplicate numbers in card4")
        testList.clear()
        testSet.clear()
        for i in range(0, len(self.card5.ns)):
            for j in range(0, len(self.card5.ns)):
                testList.append(self.card5.card[i][j])
                testSet.add(self.card5.card[i][j])
        self.assertEqual(len(testList), len(testSet), "Ensure that there are no duplicate numbers in card5")
        testList.clear()
        testSet.clear()


if __name__ == '__main__':  	    	       
    unittest.main()  	    	       
