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

from Usage import usage

def wc(files):  	    	       
    howManyFiles = len(files)
    if howManyFiles < 1:
        usage("Too few arguments", "wc")
    if howManyFiles > 1:
        totalWords = 0
        totalChars = 0
        totalLines = 0
        for fileName in files:
            numOfWords = 0
            numOfChars = 0
            fileObject = open(fileName)
            lineList = fileObject.readlines()
            for line in lineList:
                wordList = line.split()
                numOfWords += len(wordList)
                for word in wordList:
                    charList = [*word]
                    numOfChars += len(charList)
            totalWords += numOfWords
            totalChars += numOfChars
            totalLines += len(lineList)
            print("{:>8} {:>8} {:>8}   {:<}".format(len(lineList), numOfWords, numOfChars, fileName))
            fileObject.close()
        print("{:>8} {:>8} {:>8}   totals".format(totalLines, totalWords, totalChars))
    else:
        for fileName in files:
            numOfWords = 0
            numOfChars = 0
            fileObject = open(fileName)
            lineList = fileObject.readlines()
            for line in lineList:
                wordList = line.split()
                numOfWords += len(wordList)
                for word in wordList:
                    charList = [*word]
                    numOfChars += len(charList)
            print("{:>8} {:>8} {:>8}   {:<}".format(len(lineList), numOfWords, numOfChars, fileName))
            fileObject.close()
