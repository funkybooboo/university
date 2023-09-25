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

def cut(args):
    if len(args) < 1:
        usage("Too few arguments", "cut")
    if args[0] == "-f":
        fileList = args[2:]
        userChoices = args[1]
        listOfUserChoices = userChoices.split(",")
        for n in listOfUserChoices:
            if not n.isnumeric():
                usage("A comma-separated field specification is required", "cut")
        listOfUserChoices.sort()
        for fileName in fileList:
            fileObject = open(fileName)
            lineList = fileObject.readlines()
            for line in lineList:
                columnList = line.split(",")
                count = 0
                for n in listOfUserChoices:
                    if count > 0 and count < len(listOfUserChoices):
                        print(",",end="")
                    print(columnList[int(n)-1].rstrip(), end='')
                    count += 1
                print()
            fileObject.close()
    else:
        for fileName in args:
            fileObject = open(fileName)
            lineList = fileObject.readlines()
            for line in lineList:
                columnList = line.split(",")
                print(columnList[0].rstrip())
            fileObject.close()


def paste(args):  	    	       
    if len(args) < 1:
        usage("Too few arguments", "paste")
    listOfLines = []
    numOfFilesProcessed = 0
    for fileName in args:
        fileObject = open(fileName)
        lineList = fileObject.readlines()
        # initialize array
        if len(listOfLines) == 0:
            listOfLines = [""]*len(lineList)
        # resize array if needed
        if len(listOfLines) < len(lineList):
            copyListOfLines = [""]*len(lineList)
            for i in range(0, len(listOfLines)):
                copyListOfLines[i] = listOfLines[i]
            listOfLines = copyListOfLines
        # append lines in file
        for i in range(0, len(lineList)):
            if numOfFilesProcessed > 0:
                listOfLines[i] += ","
            listOfLines[i] += lineList[i].rstrip()
        numOfFilesProcessed += 1
        fileObject.close()
    # print
    for line in listOfLines:
        print(line)
