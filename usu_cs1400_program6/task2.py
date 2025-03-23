# Nate Stott
# CS1400-MO1 XL
# Assignment 6

def getUserData():
    masterList = []
    done = False
    while not done:
        userInput = input("Input: ")
        if userInput.isnumeric() == True:
            masterList.append(int(userInput))
        else:
            done = True
    return masterList

def getNumberOfElements(list):
    count = 0
    for i in list:
        count += 1
    return count

def getMaxNum(list):
    maxVal = None
    for i in list:
        if maxVal is None or i > maxVal:
            maxVal = i
    return maxVal


def getSumOfValues(list, numOfElements):
    sumOfValues = 0
    i = 0
    while i < numOfElements:
        getNum = list[i]
        sumOfValues += getNum
        i += 1
    return sumOfValues

def getAverageOfValues(sum, numOfel):
    average = sum // numOfel
    return average

def main():
    print("Add as many numbers as you want!")
    print("Once you are done entering then hit enter with no input.")
    print()

    masterList = getUserData()
    print()


    numOfElements = getNumberOfElements(masterList)
    print("Number of values entered: ", numOfElements)

    maxValue = getMaxNum(masterList)
    print("Maximum value: ", maxValue)

    minValue = min(masterList)
    print("Minimum value: ", minValue)

    sumOfValues = getSumOfValues(masterList, numOfElements)
    print("Sum of all values: ", sumOfValues)

    averageOfElements = getAverageOfValues(sumOfValues, numOfElements)
    print("Average value: ", averageOfElements)

main()

