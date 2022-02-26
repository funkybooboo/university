# Nate Stott
# CS1400-MO1 XL
# Assignment 6

from math import pi
from random import shuffle # Hint hint
from random import randint
import time

class Orbian:
    # DO NOT MODIFY THE CONSTRUCTOR
    def __init__(self, name, headRadius, bodyRadius, bodyHeight):
        # NOTE: These are constants
        self.__HEAD_RADIUS = headRadius
        self.__BODY_RADIUS = bodyRadius
        self.__BODY_HEIGHT = bodyHeight
        self.__NAME = name
        self.__BIRTH_TIME = time.time()

        # This is the only variable
        self.__adult = False

    def __getHeadVolume(self):
        return 4 / 3 * pi * self.__getHeadRadius() ** 3

    def __getBodyVolume(self):
        return pi * self.__getBodyRadius() ** 2 * self.__getBodyHeight()

    def __ageCheck(self):
        # Become an adult at 2
        if self.getAge() >= 2:
            self.__adult = True

    ####### ADD OTHER REQUIRED METHODS BELOW. SEE THE ASSIGNMENT DESCRIPTION AND OTHER STARTER CODE FOR INSIGHT ######

    def __add__(self, other):
        headRadius = (self.__HEAD_RADIUS + other.__HEAD_RADIUS) / 4
        bodyRadius = (self.__BODY_RADIUS + other.__BODY_RADIUS) / 4
        bodyHight = (self.__BODY_HEIGHT + other.__BODY_HEIGHT) / 8

        lengthOfName = (len(self.getName()) + len(other.getName())) / 2
        parent1Name = []
        parent2Name = []
        for c in self.getName():
            parent1Name.append(c)
        for c in other.getName():
            parent2Name.append(c)
        shuffle(parent1Name)
        shuffle(parent2Name)
        i = 0
        name = ""
        while i < lengthOfName:
            if len(parent1Name) > i:
                name += parent1Name[i]
            elif len(parent2Name) > i:
                name += parent2Name[i]
            i += 1
        newOrbian = Orbian(name, headRadius, bodyRadius, bodyHight)
        return newOrbian

    def __eq__(self, other):
        return self.getVolume() == other.getVolume()

    def __gt__(self, other):
        return self.getVolume() > other.getVolume()

    def __len__(self):
        return self.__BODY_HEIGHT + ((self.__HEAD_RADIUS) * 2)

    def __getHeadRadius(self):
        return self.__HEAD_RADIUS

    def __getBodyRadius(self):
        return self.__BODY_RADIUS

    def __getBodyHeight(self):
        return self.__BODY_HEIGHT

    def getAge(self):
        return time.time() - self.__BIRTH_TIME

    def getName(self):
        return self.__NAME

    def getVolume(self):
        volume = self.__getBodyVolume() + self.__getHeadVolume()
        return volume


