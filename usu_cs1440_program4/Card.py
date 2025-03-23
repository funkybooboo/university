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

class Card():  	    	       
    COLUMN_NAMES = list("BINGODARLYZEMPUX")  	    	       

    def __init__(self, idnum, ns):
        """  	    	       
        Initialize a Bingo! card  	    	       
        """
        self.idnum = idnum
        self.ns = ns
        self.card = []
        i = 0
        self.ns.shuffle()
        while i < len(self.ns):
            self.card.append(self.ns.next_row())
            i += 1
        if not len(self.ns) % 2 == 0:
            midColIndex = int((len(self.ns)-1)/2)
            midRowIndex = midColIndex
            self.card[midColIndex][midRowIndex] = "FREE!"

    def id(self):  	    	       
        """  	    	       
        Return an integer: the ID number of the card  	    	       
        """  	    	       
        return self.idnum

    def number_at(self, row, col):  	    	       
        """  	    	       
        Return an integer or a string: the value in the Bingo square at (row, col)  	    	       
        """
        return self.card[row][col]

    def __len__(self):  	    	       
        """  	    	       
        Return an integer: the length of one dimension of the card.  	    	       
        For a 3x3 card return 3, for a 5x5 return 5, etc.  	    	       

        This method was called `size` in the C++ version  	    	       
        """  	    	       
        return len(self.ns)

    def __str__(self):  	    	       
        """  	    	       
        Return a string: a neatly formatted, square bingo card  	    	       

        This is basically equivalent to the `operator<<` method in the C++ version  	    	       
        """

        s = ""
        for i in range(0, len(self.ns)):
            s += "   " + self.COLUMN_NAMES[i] + "  "
        s += "\n"
        s += self.__make_row_sep()
        for i in range(0, len(self.ns)):
            row = self.card[i]
            for j in range(0, len(row)):
                if row[j] == "FREE!":
                    s += "|" + row[j]
                    continue
                s += "| " + self.__get_cell_padding(row[j])
            s += "|\n"
            s += self.__make_row_sep()
        return s

    def __get_cell_padding(self, n):
        s = ""
        lengthOfNum = len(str(n))
        if lengthOfNum == 1:
            return " " + str(n) + "  "
        s += str(n)
        for i in range(lengthOfNum, 4):
            s += " "
        return s

    def __make_row_sep(self):
        s = ""
        rowSep = "+-----"
        for i in range(0, len(self.ns)):
            s += rowSep
        return s + "+\n"
