#!/usr/bin/env python

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


import os, sys

def sendError(string=""):
    if string == "":
        string = "Error! The supplied text file does not exist or is not accessible."
    print(f"""\
================================================================================
{string}
================================================================================
""")
    sys.exit(1)

def getFiles(textFile=""):
    if textFile == "":
        textFile = input("Please select a text file to parse: ")
    if not os.access(textFile, os.R_OK):
        sendError(string=f"Error! The supplied text file {textFile} does not exist or is not accessible.")
    file = open(textFile)
    return file

def convertLower(char):
    startLetterOrd = ord("a")
    num = ord(char)
    return "_" + str(num - startLetterOrd)

def convertUpper(char):
    startLetterOrd = ord("A")
    num = ord(char)
    return "^" + str(num - startLetterOrd)

def convertSpecialChar(char):
    charOrd = ord(char)
    # range for (" " to "/"), ords (32 to 47)
    if charOrd >= ord(" ") and charOrd <= ord("@"):
        base = ord(" ")
        diff = charOrd - base
        return "+A" + str(diff)
    elif charOrd >= ord("[") and charOrd <= ord("`"):
        base = ord("[")
        diff = charOrd - base
        return "+B" + str(diff)
    elif charOrd >= ord("{") and charOrd <= ord("~"):
        base = ord("{")
        diff = charOrd - base
        return "+C" + str(diff)
    else:
        # Python evaluates`None` to be `False`.
        return None

def cryptLine(line):
    output = ""
    line = line.rstrip()
    for char in line:
        specChar = convertSpecialChar(char)
        if char.isupper():
            output += convertUpper(char) + " "
        elif char.islower():
            output += convertLower(char) + " "
        elif specChar != None:
            output += specChar + " "
        else:
            sendError(f"Error! the character {char} is not a valid character to be crypted.")
    return output


def main():
    if len(sys.argv) < 2:
        line = input("Please enter something to crypt: ")
        print(cryptLine(line))
    if len(sys.argv) == 2:
        if sys.argv[1].lower() == '-h' or sys.argv[1].lower() == '--help':
            print(f"""\
USAGE: $ python duckieEncrypter.py [FILE]

[FILE] is an optional argument that is the path to a text file to encrypt.

If [FILE] is not given, the user is prompted to enter a single line of text
    input to encrypt.""")
        else:
            file = getFiles(sys.argv[1])
            for line in file.readlines():
                line = cryptLine(line)
                print(line)
            file.close()


if __name__ == '__main__':
    main()
