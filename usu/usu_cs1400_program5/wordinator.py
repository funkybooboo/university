# Nate Stott
# CS1400-MO1 XL
# Assignment 5

class Wordinator:

    def __init__(self, word):
        self.__word = str(word)

    def __add__(self, other):
        return self.__word + other.__word

    def __lt__(self, other):
        return self.__word < other.__word

    def __str__(self):
        return str(self.__word)

    def __mul__(self, factor):
        return self.__word * factor

    def __truediv__(self, other):
        i = 0
        newWord = ""
        m = max(len(self.__word), len(other.__word))

        while i <= m:
            if i < len(self.__word):
                newWord += self.__word[i]
            if i < len(other.__word):
                newWord += other.__word[i]
            i += 1

        return newWord

    def __mod__(self, other):
        word1 = self.__word
        word2 = other.__word
        # DonaldDuck
        # aldD
        # MickeyMouse
        # keyMo
        word1 = self.getMiddle(word1)
        word2 = self.getMiddle(word2)
        return word1, word2

    def getMiddle(self, word):
        step1 = len(word) // 2
        step2 = len(word) - step1
        isEven = step2 % 2 == 0
        if isEven:
            offset = step2 // 2
            length = step1
        else:
            offset = (step2 - 1) // 2
            length = step1

        word = word[offset - 1: length]
        return word

    def __sub__(self, other):
        orgWord1 = self.__word
        orgWord2 = other.__word
        i = 0
        j = 0
        newWord1 = ""
        newWord2 = ""

        while i < len(orgWord1):
            getLetter1 = orgWord1[i]

            if getLetter1 == "A":
                newWord1 += "a"
            elif getLetter1 == "a":
                newWord1 += "A"
            elif getLetter1 == "B":
                newWord1 += "b"
            elif getLetter1 == "b":
                newWord1 += "B"
            elif getLetter1 == "C":
                newWord1 += "c"
            elif getLetter1 == "c":
                newWord1 += "C"
            elif getLetter1 == "D":
                newWord1 += "d"
            elif getLetter1 == "d":
                newWord1 += "D"
            elif getLetter1 == "E":
                newWord1 += "e"
            elif getLetter1 == "e":
                newWord1 += "E"
            elif getLetter1 == "F":
                newWord1 += "f"
            elif getLetter1 == "f":
                newWord1 += "F"
            elif getLetter1 == "G":
                newWord1 += "g"
            elif getLetter1 == "g":
                newWord1 += "G"
            elif getLetter1 == "H":
                newWord1 += "h"
            elif getLetter1 == "h":
                newWord1 += "H"
            elif getLetter1 == "I":
                newWord1 += "i"
            elif getLetter1 == "i":
                newWord1 += "I"
            elif getLetter1 == "J":
                newWord1 += "j"
            elif getLetter1 == "j":
                newWord1 += "J"
            elif getLetter1 == "K":
                newWord1 += "k"
            elif getLetter1 == "k":
                newWord1 += "K"
            elif getLetter1 == "L":
                newWord1 += "l"
            elif getLetter1 == "l":
                newWord1 += "L"
            elif getLetter1 == "M":
                newWord1 += "m"
            elif getLetter1 == "m":
                newWord1 += "M"
            elif getLetter1 == "N":
                newWord1 += "n"
            elif getLetter1 == "n":
                newWord1 += "N"
            elif getLetter1 == "O":
                newWord1 += "o"
            elif getLetter1 == "o":
                newWord1 += "O"
            elif getLetter1 == "P":
                newWord1 += "p"
            elif getLetter1 == "p":
                newWord1 += "P"
            elif getLetter1 == "Q":
                newWord1 += "q"
            elif getLetter1 == "q":
                newWord1 += "Q"
            elif getLetter1 == "R":
                newWord1 += "r"
            elif getLetter1 == "r":
                newWord1 += "R"
            elif getLetter1 == "S":
                newWord1 += "s"
            elif getLetter1 == "s":
                newWord1 += "s"
            elif getLetter1 == "T":
                newWord1 += "t"
            elif getLetter1 == "t":
                newWord1 += "T"
            elif getLetter1 == "U":
                newWord1 += "u"
            elif getLetter1 == "u":
                newWord1 += "U"
            elif getLetter1 == "V":
                newWord1 += "v"
            elif getLetter1 == "v":
                newWord1 += "V"
            elif getLetter1 == "W":
                newWord1 += "w"
            elif getLetter1 == "w":
                newWord1 += "W"
            elif getLetter1 == "X":
                newWord1 += "x"
            elif getLetter1 == "x":
                newWord1 += "X"
            elif getLetter1 == "Y":
                newWord1 += "y"
            elif getLetter1 == "y":
                newWord1 += "Y"
            elif getLetter1 == "Z":
                newWord1 += "z"
            elif getLetter1 == "z":
                newWord1 += "Z"
            i += 1

        while j < len(orgWord2):
            getLetter2 = orgWord2[j]

            if getLetter2 == "A":
                newWord2 += "a"
            elif getLetter2 == "a":
                newWord2 += "A"
            elif getLetter2 == "B":
                newWord2 += "b"
            elif getLetter2 == "b":
                newWord2 += "B"
            elif getLetter2 == "C":
                newWord2 += "c"
            elif getLetter2 == "c":
                newWord2 += "C"
            elif getLetter2 == "D":
                newWord2 += "d"
            elif getLetter2 == "d":
                newWord2 += "D"
            elif getLetter2 == "E":
                newWord2 += "e"
            elif getLetter2 == "e":
                newWord2 += "E"
            elif getLetter2 == "F":
                newWord2 += "f"
            elif getLetter2 == "f":
                newWord2 += "F"
            elif getLetter2 == "G":
                newWord2 += "g"
            elif getLetter2 == "g":
                newWord2 += "G"
            elif getLetter2 == "H":
                newWord2 += "h"
            elif getLetter2 == "h":
                newWord2 += "H"
            elif getLetter2 == "I":
                newWord2 += "i"
            elif getLetter2 == "i":
                newWord2 += "I"
            elif getLetter2 == "J":
                newWord2 += "j"
            elif getLetter2 == "j":
                newWord2 += "J"
            elif getLetter2 == "K":
                newWord2 += "k"
            elif getLetter2 == "k":
                newWord2 += "K"
            elif getLetter2 == "L":
                newWord2 += "l"
            elif getLetter2 == "l":
                newWord2 += "L"
            elif getLetter2 == "M":
                newWord2 += "m"
            elif getLetter2 == "m":
                newWord2 += "M"
            elif getLetter2 == "N":
                newWord2 += "n"
            elif getLetter2 == "n":
                newWord2 += "N"
            elif getLetter2 == "O":
                newWord2 += "o"
            elif getLetter2 == "o":
                newWord2 += "O"
            elif getLetter2 == "P":
                newWord2 += "p"
            elif getLetter2 == "p":
                newWord2 += "P"
            elif getLetter2 == "Q":
                newWord2 += "q"
            elif getLetter2 == "q":
                newWord2 += "Q"
            elif getLetter2 == "R":
                newWord2 += "r"
            elif getLetter2 == "r":
                newWord2 += "R"
            elif getLetter2 == "S":
                newWord2 += "s"
            elif getLetter2 == "s":
                newWord2 += "s"
            elif getLetter2 == "T":
                newWord2 += "t"
            elif getLetter2 == "t":
                newWord2 += "T"
            elif getLetter2 == "U":
                newWord2 += "u"
            elif getLetter2 == "u":
                newWord2 += "U"
            elif getLetter2 == "V":
                newWord2 += "v"
            elif getLetter2 == "v":
                newWord2 += "V"
            elif getLetter2 == "W":
                newWord2 += "w"
            elif getLetter2 == "w":
                newWord2 += "W"
            elif getLetter2 == "X":
                newWord2 += "x"
            elif getLetter2 == "x":
                newWord2 += "X"
            elif getLetter2 == "Y":
                newWord2 += "y"
            elif getLetter2 == "y":
                newWord2 += "Y"
            elif getLetter2 == "Z":
                newWord2 += "z"
            elif getLetter2 == "z":
                newWord2 += "Z"

            j += 1

        return newWord1, newWord2

    def backWordSlice(self):
        word = self.__word
        backwardsWord = word[:: -1]

        return backwardsWord

    def backWordManual(self):
        word = self.__word
        backwardsWord = ""
        i = -1
        while i >= (-1 * len(word)):
            getLetter = word[i]
            backwardsWord += getLetter
            i -= 1

        return backwardsWord
