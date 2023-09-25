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


# Feel free to start from scratch, or repurpouse any of these suggested  	    	       
#   functions! The world is yours. Well, maybe that was a bit of an over  	    	       
#   exaggeration... The world isn't only *yours*, but this file sure is.  	    	       
# Okay, I actually lied. Please keep the stuff management asks you to at the  	    	       
#   bottom of the file, in the if __name__ == "__main__": block.
import os
import sys
from os import getcwd # do I want anything else from os? Maybe access and R_OK?

def sendError(msg=None, exitCode=1):  	    	       
    '''  	    	       
DESCRIPTION:  	    	       
  Exits the program after printing an error message.  	    	       

PARAMETERS:  	    	       
  msg : None | str  	    	       
    The message to print in the error message.  	    	       
    If no `msg` is given, the default message becomes "Error! An error was  	    	       
        encountered, so the program is quitting."  	    	       
  exitCode : int  	    	       
    The exit code to exit the program with.  	    	       
    If no exitCode argument is given, it defaults to 1.  	    	       

RETURNS:  	    	       
  Nothing, program quits with exit code `exitCode`  	    	       
    '''  	    	       
    # Dear Future Dev,  	    	       
    # The code below is fine. Your work is not needed on `sendError`.  	    	       
    # You are more than welcome to edit the string literal, especially to make  	    	       
    # a more vocal and unique quack.  	    	       
    if msg is None:  	    	       
        msg = "Error! The supplied text file does not exist or is not accessible."
    print(f""" 	    	       
!!!QUACK!!!  	    	       
================================================================================  	    	       
{msg}  	    	       
================================================================================  	    	       
!!!QUACK!!!  	    	       
""")  	    	       
    sys.exit(exitCode)  	    	       


def convertToLower(charCode):  	    	       
    '''  	    	       
DESCRIPTION:  	    	       
  Convert the lowercase DuckieCrypt character with char code `charCode` into a  	    	       
    single plain-text lowercase character, if the char code is valid.  	    	       

PARAMETERS:  	    	       
  charCode : str  	    	       
    The character code of the DuckieCrypt lowercase character to decrypt.  	    	       

RETURNS:  	    	       
  Returns a single decrypted character OR an empty string.  	    	       
    '''  	    	       
    # if charCode.isdigit(): ...
    charCode = int(charCode)
    if 0 <= charCode <= 25:
        return chr(charCode + 97)
    return ''


def convertToUpper(charCode):
    '''  	    	       
DESCRIPTION:  	    	       
  Convert the uppercase DuckieCrypt character with char code `charCode` into a  	    	       
    single plain-text uppercase character, if the char code is valid.  	    	       

PARAMETERS:  	    	       
  charCode : str  	    	       
    The character code of the DuckieCrypt uppercase character to decrypt.  	    	       

RETURNS:  	    	       
  Returns a single decrypted character OR an empty string.  	    	       
    '''
    charCode = int(charCode)
    if 0 <= charCode <= 25:
        return chr(charCode + 65)
    return ''


def convertToSpecialChar(charCode):  	    	       
    '''  	    	       
DESCRIPTION:  	    	       
  Convert the special DuckieCrypt character with char code `charCode` into a  	    	       
    single plain-text special character, if the char code is valid.  	    	       

PARAMETERS:  	    	       
  charCode : str  	    	       
    The character code of the DuckieCrypt special character to decrypt.  	    	       

RETURNS:  	    	       
  Returns a single decrypted character OR an empty string.  	    	       
    '''
    if charCode[1:] == '':
        return ''
    if charCode[0] == 'A' and (0 <= int(charCode[1:]) <= 3):
        return chr(int(charCode[1:]) + 32)
    elif charCode[0] == 'B' and (0 <= int(charCode[1:]) <= 5):
        return chr(int(charCode[1:]) + 91)
    elif charCode[0] == 'C' and (0 <= int(charCode[1:]) <= 32):
        return chr(int(charCode[1:]) + 123)
    return ''


def decryptCharacter(character):  	    	       
    '''  	    	       
DESCRIPTION:  	    	       
  Decrypts a single DuckieCrypt character. Returns a single character.  	    	       

PARAMETERS:  	    	       
  character : str  	    	       
    The DuckieCrypt character to decrypt.  	    	       

RETURNS:  	    	       
  A single character that is the decrypted DuckieCrypt character OR an empty  	    	       
    string.  	    	       

    '''  	    	       
    # HINT: "^23"[1:] == "23"  	    	       
    convertedChar = ''
    if character[0] == '^':
        convertedChar = convertToUpper(character[1:])
    elif character[0] == '_':
        convertedChar = convertToLower(character[1:])
    elif character[0] == '+':
        convertedChar = convertToSpecialChar(character[1:])
    if not convertedChar == '':
        return convertedChar
    return ''


def decryptLine(line):  	    	       
    '''  	    	       
DESCRIPTION:  	    	       
  Decrypt a single line of DuckieCrypt.  	    	       

PARAMETERS:  	    	       
  line : str  	    	       
    The line of DuckieCrypt to decrypt.  	    	       

RETURNS:  	    	       
  A string that is the decrypted text.  	    	       
    '''
    output = ''
    list = line.split()
    for char in list:
        decryptedChar = ''
        decryptedChar += decryptCharacter(char)
        if not decryptedChar == '':
            output += decryptedChar
    return output


def getFile(pathToFile):  	    	       
    '''  	    	       
DESCRIPTION:  	    	       
  Checks the `pathToFile` that was given. If it exists, return a file object  	    	       
    for that file. Otherwise, exit the program with a message indicating the  	    	       
    issue.  	    	       

PARAMETERS:  	    	       
  pathToFile : str  	    	       
    The file path for the file that should be opened and decrypted  	    	       

RETURNS:  	    	       
  An opened file object OR quits program, not returning anything  	    	       
    '''
    if os.access(pathToFile, os.R_OK):
        return open(pathToFile)
    else:
        sendError()


def main(filePath):  	    	       
    '''  	    	       
DESCRIPTION:  	    	       
  Run the main logic of the DuckieDecrypter program. Decrypts a given file, if  	    	       
    it exists, printing out the result.  	    	       

PARAMETERS:  	    	       
  filePath : str  	    	       
    The relative or absolute path to the file to decrypt  	    	       

RETURNS:  	    	       
  Nothing. Just prints out the decrypted text.  	    	       
    '''  	    	       
    ### MANAGEMENT COMMENT:  	    	       
    ###   This function is relied on in the code provided by management below.  	    	       
    ###   If you change this functions name or parameters, be sure to update  	    	       
    ###     the provided code below so it works.  	    	       
    file = getFile(filePath)
    list = file.readlines()
    for line in list:
        print(decryptLine(line))
    file.close()


# This block of code in the `if __name__ == "__main__":` block will *only* be  	    	       
#   executed if this duckieDecrypter.py file is run as the *main* file. Not if  	    	       
#    it's imported! It's the entry point to our program. Neat, right?  	    	       
# You'll probably see this in a lot of Python code out there.  	    	       

if __name__ == '__main__':  	    	       

    ### DUCKIECORP MANAGEMENT COMMENT:  	    	       
    ###   Do not modify the code after this comment, unless otherwise noted.  	    	       
    ###   The following code has been explained in the comments for you.  	    	       

    # The user asks for a help message by giving the argument '-h' or '--help'  	    	       
    if "-h" in sys.argv or "--help" in sys.argv:  	    	       
        # This is a block string used to make the error message  	    	       
        MSG = f"""\  	    	       
USAGE:  	    	       
  $ python {sys.argv[0]} [FILE PATH]  	    	       

DESCRIPTION:  	    	       
  The DuckieDecrypter is a proprietary tool created by DuckieCorp to *decrypt*  	    	       
    messages that are composed of DuckieCrypt. That is, to turn DuckieCrypt  	    	       
    back into plain text.  	    	       

ARGUMENTS:  	    	       
  [FILE PATH] : Optional  	    	       
    Specify the path to a file containing DuckieCrypt to decrypt.  	    	       
    If this argument is not given, then the user is prompted for manual input  	    	       
      for the file path.  	    	       
    Only one file can be specified.  	    	       

  -h | --help  	    	       
    Produce this help message if given as any argument to the program.  	    	       
"""  	    	       
        print(MSG, end='')  	    	       
        # Program quits here if the usage message is asked for  	    	       
        sys.exit(0)  	    	       
    # If the file path argument was given...  	    	       
    elif len(sys.argv) > 1:  	    	       
        # Extract the file path argument from the command line  	    	       
        filePath = sys.argv[1]  	    	       
    # No arguments were given...  	    	       
    else:  	    	       
        # So we manually prompt the user for the file  	    	       
        print(f"Your current working directory is:\n  {getcwd()}")
        filePath = input("File to decrypt: ")  	    	       

    # Actually run the logic of the DuckieDecrypter by decrypting the file at  	    	       
    #   the location `filePath`  	    	       
    # You *can* modify this single line if you modify `main` above.  	    	       
    main(filePath)  	    	       
