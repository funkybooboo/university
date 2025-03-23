# Nate Stott
# CS1400-MO1 XL
# Assignment 3

# open the test
print("Welcome to the Social Situation Analyzer System")

# get user ones input
print("Person One")
personOneName = input("\t Enter your name: ")
personOnePos = input("\t Enter your position (x, y):  ")
personOneRad = float(input("\t Enter your personal space radius: "))

# split person ones x and y
personOneSplit = personOnePos.split(",")
personOneX = float(personOneSplit[0].strip())
personOneY = float(personOneSplit[1].strip())

# get user twos input
print("Person Two")
personTwoName = input("\t Enter your name: ")
personTwoPos = input("\t Enter your position (x, y):  ")
personTwoRad = float(input("\t Enter your personal space radius: "))

# split person twos x and y
personTwoSplit = personTwoPos.split(",")
personTwoX = float(personTwoSplit[0].strip())
personTwoY = float(personTwoSplit[1].strip())

# calculation
theDistance = ((((personTwoX - personOneX) ** 2) + ((personTwoY - personOneY) ** 2)) ** 0.5)

# start testing
msg = "\nSocial Situation Analysis Results\n"

# Person Test: This test determines if an individual has someone in their own personal space. One of the following outputs is possible:
msg += "\t"

# Person One is in Person Two's personal space
if theDistance >= personTwoRad:
    msg += personOneName + " is in " + personTwoName + "'s personal space"
# Person Two is in Person One's personal space
elif theDistance <= personOneRad:
    msg += personTwoName + " is in " + personOneName + "'s personal space"
# Person One and Person Two are in each other's personal space
elif theDistance <= personOneRad and theDistance <= personTwoRad:
    msg += personOneName + " and " + personTwoName + "are in each other's personal space"
# Neither Person One nor Person Two is in the other's personal space
elif theDistance > personOneRad and theDistance > personTwoRad:
    msg += "Neither " + personOneName + " nor " + personTwoName +  "is in the other's personal space"
else:
    msg += "Couldn't determine \"Person Test\""

msg += "\n"
# Space Test: This test determines the relation of the personal spaces of each person. One the following outputs is possible:
msg += "\t"

# Person One and Person Two's personal spaces overlap
if theDistance - personOneRad - personTwoRad <= 0:
    msg += personOneName + " and " + personTwoName + "'s personal spaces overlap"
# Person One and Person Two's personal spaces do not overlap
elif theDistance - personOneRad - personTwoRad > 0:
    msg += personOneName + " and " + personTwoName + "'s personal spaces do not overlap"
# Person One's personal space is entirely inside Person Two's personal space
elif theDistance + personOneRad < personTwoRad:
    msg += personOneName + "'s personal space is entirely inside " + personTwoName + "'s personal space"
# Person Two's personal space is entirely inside Person One's personal space
elif theDistance + personTwoRad < personOneRad:
    msg += personTwoName + "'s personal space is entirely inside " + personOneName + "'s personal space"
else:
    msg += "Couldn't determine \"Space Test\""

# print message
print(msg)
