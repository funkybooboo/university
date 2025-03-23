# Nate Stott
# CS1400-MO1 XL
# Assignment 7

from random import randint

# Remember how to use this kind of variable?
count = 0

def main():
    print("Welcome to Recursion Fun")

    # aggienacci calculation
    value = eval(input("Enter a number to find it's aggienacci value: "))
    print("The aggienacci value of " + str(value) + " is " + str(round(aggienacci(value), 4)))

    print()

    # Recursive search and sort
    numList = []
    for i in range(200000):
        if randint(0, 2) == 0:
            numList.append(i)
    numList = bubbleSort(numList)
    key = eval(input("Enter a number to search for: "))
    numPos = binarySearch(numList, key)
    # makes a list of number bewteen 1 and 200,00
    # cuts out half of the numbers randomly
    # List gets sorted from least to greatest

    if numPos == -1:
        print("Your number, " + str(key) + ", is not in the list")
    else:
        print("Your number, " + str(key) + ", is in the list at position " + str(numPos))

    print("Total recursive calls: " + str(count))

def aggienacci(value):
    # ag(0) = 0
    # ag(1) = 1
    # ag(2) = 2
    # ag(x) = (ag(x-3) + ag(x-2)) / ag(x-1)
    if value == 0:
        return 0
    elif value == 1:
        return 1
    elif value == 2:
        return 2
    else:
        return round((aggienacci(value - 3) + aggienacci(value - 2) / aggienacci(value - 1)), 2)

def binarySearch(numList, key):
    # Need to find the key inside of the numList
    # look at the middle value in the list and see if the key is less then or greater then the middle value
    # cut out half the list
    # do it again until the number for the key is found
    low = 0
    high = len(numList) - 1
    return doBinarySearch(numList, key, low, high)

def doBinarySearch(numList, key, low, high):
    if key == numList[low]:
        return low
    elif key == numList[high]:
        return high
    elif low >= high:
        return -1
    elif high <= low:
        return -1
    else:
        midPoint = (high + low) // 2
        valAtMidPoint = numList[midPoint]
        if key == valAtMidPoint:
            return midPoint
        elif key > valAtMidPoint:
            low = midPoint + 1
            return doBinarySearch(numList, key, low, high)
        elif key < valAtMidPoint:
            high = midPoint - 1
            return doBinarySearch(numList, key, low, high)
        else:
            return -1

def bubbleSort(listOfRank):
    didSwap = True
    while didSwap:
        didSwap = False
        sortCnt = 1
        for i in range(len(listOfRank) - sortCnt):
            if listOfRank[i] > listOfRank[i + 1]:
                listOfRank[i], listOfRank[i + 1] = listOfRank[i + 1], listOfRank[i]
                didSwap = True
        sortCnt += 1
    return listOfRank


main()


