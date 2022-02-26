# Nate Stott
# CS1400-MO1 XL
# Assignment 3

from random import randint
from random import seed
import time

# start timer
start = time.time()

# create varables to start off
flukyNumCount = 0
count = 0

# start the for loop and tests
for count in range(1, 10001):
    # make varables for the tests
    theNumber = count
    theSeed = 0
    value = 1
    # go threw all numbers and get there factors
    while value < theNumber:
        if theNumber % value == 0:
            seed(value)
            theSeed += randint(1, theNumber)
        value += 1
    # found a fluky number
    if theSeed == theNumber:
        print("Fluky Number: " + str(theNumber))
        flukyNumCount += 1
    # once 7 are found then end the tests
    if flukyNumCount == 7:
        break

# end program and print out information with correct formating
duration = time.time()
timePassed = duration - start
msg = "Total Time: " + '{:.2f}'.format(timePassed) + " seconds" + "\n"
msg += "Total Loops: " + str(count)
print(msg)
