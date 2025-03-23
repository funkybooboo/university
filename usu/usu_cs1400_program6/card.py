# Nate Stott
# CS1400-MO1 XL
# Assignment 6

from gronkyutil import TITLE, GANG

class Card:
    def __init__(self, id):
        self.__id = id

    def getTitle(self):
        return TITLE[self.__id % len(TITLE)]

    def getGang(self):
        return GANG[self.__id // len(TITLE)]

    def __repr__(self):
        return  "\t\t" + self.getTitle() + " of " + self.getGang()

    def getID(self):
        return self.__id
