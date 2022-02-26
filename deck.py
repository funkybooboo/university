# Nate Stott
# CS1400-MO1 XL
# Assignment 6

from card import Card
import random
from gronkyutil import TITLE, GANG

class Deck:

    def __init__(self):
        self.shuffle()

    def shuffle(self):
        self.__deck = []
        for i in range(len(TITLE) * len(GANG)):
            self.__deck.append(Card(i))
        random.shuffle(self.__deck)

    def draw(self):
        return self.__deck.pop()
