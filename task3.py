# Nate Stott
# CS1400-MO1 XL
# Assignment 2

import math

# get user data
numberOfSides = float(input("Enter the length of the sides: "))
lengthOfEachSide = float(input("Enter the number of sides: "))

# do calculation
area = (numberOfSides * (lengthOfEachSide ** 2)) / (4 * math.tan(math.pi / numberOfSides))

# round
roundedArea = round(area, 5)

# give user awnser
print("The Area of the regular polygon: ", roundedArea)
