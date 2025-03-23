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

def head(args):
    numOfLines = 10
    howManyFileNames, listOfFileNames, numOfLines = getFileNameData(args, numOfLines, "head")
    printData(howManyFileNames, listOfFileNames, numOfLines, "head")

def printData(howManyFileNames, listOfFileNames, numOfLines, option):
    if howManyFileNames > 1:
        for fileName in listOfFileNames:
            fileObject = open(fileName)
            lineList = fileObject.readlines()
            print(f"==> {fileName} <==")
            if option == "head":
                printForHead(lineList, numOfLines)
            else:
                printForTail(lineList, numOfLines)
            fileObject.close()
    else:
        for fileName in listOfFileNames:
            fileObject = open(fileName)
            lineList = fileObject.readlines()
            if option == "head":
                printForHead(lineList, numOfLines)
            else:
                printForTail(lineList, numOfLines)
            fileObject.close()

def printForTail(lineList, numOfLines):
    lastIndex = len(lineList)
    startIndex = lastIndex - numOfLines
    if startIndex < 0:
        startIndex = 0
    for line in range(startIndex, lastIndex):
        print(lineList[line],end='')

def printForHead(lineList, numOfLines):
    lastIndex = len(lineList)
    if lastIndex < numOfLines:
        for line in range(0, lastIndex):
            print(lineList[line], end='')
    else:
        for line in range(0, numOfLines):
            print(lineList[line], end='')

def getFileNameData(args, numOfLines, option):
    if args[0] == "-n":
        if len(args[1:]) < 1:
            usage("Too few arguments", option)
        if not args[1].isnumeric():
            usage("No numeric arguments with the -n flag", option)
        numOfLines = int(args[1])
        howManyFileNames = len(args[2:])
        listOfFileNames = args[2:]
    else:
        howManyFileNames = len(args)
        listOfFileNames = args
    return howManyFileNames, listOfFileNames, numOfLines

def tail(args):
    numOfLines = 10
    howManyFileNames, listOfFileNames, numOfLines = getFileNameData(args, numOfLines, "tail")
    printData(howManyFileNames, listOfFileNames, numOfLines, "tail")
