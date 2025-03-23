# Nate Stott
# CS1400-MO1 XL
# Assignment 7

class MemoryCard:
    # Symbols are ascii characters starting with "!". Check out https://www.asciitable.com/
    # for a refresher. Don't forget functions we've learned to encode/decode ascii values.
    # You may include any private data and methods that you see fit.
    # Hint: I would build this first, then test it with the code in test.py.

    def __init__(self, idNum):
        self.idNum = idNum
        self.value = str(chr((idNum // 2) + 33))
        self.flipped = False

    def getValue(self):
        # returns a value associated with a card. Note, this is not the same as the id as matching cards will have the same value
        return self.value

    def toggleFlipped(self):
        #  change the card from a face-up to a face-down, and vice versa
        self.flipped = not self.flipped

    def isFlipped(self):
        # returns true if the card is currently face-up
        return self.flipped

    def displayCard(self):
        #  returns a string. If the card is face-up it returns a symbol associated with the value.
        #  If it is face-down, it returns a space.
        #  If you don't return a space then your game board printout will be kinda messed up.
        if self.flipped:
            return self.value
        else:
            return " "

