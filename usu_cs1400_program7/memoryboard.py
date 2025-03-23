# Nate Stott
# CS1400-MO1 XL
# Assignment 7

from modules.memorycard import MemoryCard
from random import shuffle

class MemoryBoard:
    # The game board should place the cards in a random order. We have done this multiple times in class.
    # You may include any private data and methods that you see fit.
    # Hint: Make sure your class works for different combinations of column and row counts (the board doesn't have to be a square).

    def __init__(self, numOfRows, numOfCols):
        self.numOfRows = numOfRows
        self.numOfCols = numOfCols
        a = []
        for i in range(numOfRows * numOfCols):
            a.append(i)
        shuffle(a)
        self.board = [[MemoryCard(a.pop()) for r in range(numOfRows)] for c in range(numOfCols)]


    def getBoard(self):
        # Example of what the gameboard should look like

        #    Player 1: 0
        #    Player 2: 0
        #    Player 3: 0
        #           Rows
        #      | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
        #    -----------------------------------
        #    1 |   |   | ? |   |   |   |   |   |
        #    -----------------------------------
        # C  2 |   |   |   |   |   |   |   |   |
        # o  -----------------------------------
        # l  3 |   |   |   |   |   | 6 |   |   |
        # s  -----------------------------------
        #    4 |   |   |   |   |   |   |   |   |
        #    -----------------------------------
        #    5 |   |   |   |   |   |   |   |   |
        #    -----------------------------------
        #    6 |   |   |   |   |   |   |   |   |
        #    -----------------------------------
        #    7 |   |   |   |   |   |   |   |   |
        #    -----------------------------------
        #    8 |   |   |   |   |   |   |   |   |
        #    -----------------------------------

        # returns a single string representation of the game board.
        # There is an example above, and it can be seen in the demo video.
        # Note that the GameBoard class does not call print(), it just returns a string that can be printed.


        msg = "  |"
        for c in range(self.numOfCols):
            msg += " " + str(c + 1) + " |"
        msg += "\n"
        msg += self.dashLayer()
        for r in range(self.numOfRows):
            msg += str(r + 1) + " |"
            for c in range(self.numOfCols):
                msg += " " + self.board[r][c].displayCard() + " |"
            msg += "\n"
            msg += self.dashLayer()

        return msg

    def dashLayer(self):
        msg = "----"
        for c in range(self.numOfCols):
            msg += "----"
        msg += "\n"
        return msg

    def flipCard(self, xPos, yPos):
        # takes an x, y coordinate from the game board and flips the card at that location.
        self.board[yPos-1][xPos-1].toggleFlipped()

    def isCardFlipped(self, xPos, yPos):
        # returns true if the card at that position is face-up, otherwise returns false
        return self.board[yPos-1][xPos-1].isFlipped()

    def isMatch(self, pos1, pos2):
        # pos1 and pos2 are the x, y locations of two different cards given as a list.
        # For example [5, 3] could be the actual value associated with pos1.
        pos1Val = self.board[pos1[1]-1][pos1[0]-1].getValue()
        pos2Val = self.board[pos2[1]-1][pos2[0]-1].getValue()
        return pos1Val == pos2Val
