# Nate Stott
# CS1400-MO1 XL
# Assignment 4

def makeNumberPyramid(numOfRows):
    # make emty message to add too
    msg = ""
    # amount of numbers I need to add
    currentNum = 1
    length = len(str(numOfRows))
    count = numOfRows



    # add the right num of nums
    while currentNum <= numOfRows:
        i = 0
        # line im working on
        currentLine = ""
        lengthOfNum = len(str(currentNum))

        # add the right num of spaces
        while i < count - 1:
            currentLine += " "
            i += 1

        muliplier = length - lengthOfNum
        i = 0

        while i < muliplier * 5:
            currentLine += " "
            i += 1

        currentLine += (str(currentNum) + " ") * currentNum
        currentLine += "\n"
        currentNum += 1
        msg += currentLine
        count -= 1

    return msg

def main():
    numOfRows = int(input("How many rows: "))
    result = (makeNumberPyramid(numOfRows))
    print(result)

main()