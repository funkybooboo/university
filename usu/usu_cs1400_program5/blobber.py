# Nate Stott
# CS1400-MO1 XL
# Assignment 5

from math import pi
import time

class Blobber:
    def __init__(self, name, color, radius, height):
        self.__radius = float(radius)
        self.__height = float(height)
        self.__color = str(color).lower()
        self.__name = str(name).capitalize()
        self.__lastUpdate = time.time()
        self.__orignalRadius = self.__radius

    def getColor(self):
        return self.__color

    def getName(self):
        return self.__name

    def setRadius(self, radius):
        self.__radius = radius

    def setHeight(self, height):
        self.__height = height

    def setColor(self, color):
        self.__color = color

    def setName(self, name):
        self.__name = name

    def feedBlobber(self, value):
        self.__radius += value
        self.vitalsOK()

    def blobberSpeak(self):
        msg = ""
        msg += "My name is " + self.__name + ", and I am " + self.__color + "\n"
        msg += "My current happiness level is " + str(format(self.vitalsOK()[0], ".2%"))
        return msg

    def vitalsOK(self):
        currentTime = time.time()

        alapsedTime = currentTime - self.__lastUpdate

        self.__radius -= (self.__orignalRadius * 0.002) * alapsedTime

        volumeAtBirth = pi * self.__orignalRadius ** 2 * self.__height
        workingVolume = pi * self.__radius ** 2 * self.__height

        happness = (workingVolume / volumeAtBirth) * 100

        if happness > 90 and happness < 110:
            return happness / 100, True
        else:
            return happness / 100, False
