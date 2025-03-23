# Nate Stott
# CS1400-MO1 XL
# Assignment 3

# get random num
import random

# create random number and print opening remarks
correctNum = random.randint(1, 10)
keepGoing = True
msg = "Welcome to the Guessing Game"
print(msg)
msg = "The Computer has picked a number from 1 - 10. Try to match it."
print(msg)

# start the game
while keepGoing:
    msg = "What number do you choose (1-10): "
    userGuess = int(input(msg))
    # check for valid input
    if userGuess > 10 or userGuess < 1:
        msg = "(1-10) please \n"
        print(msg)
    # enter to check the result of the guess
    else:
        # got it right and end game
        if userGuess == correctNum:
            msg = "Exact match: Honored to play with you, Master. \n"
        # got it wrong and find the right message
        else:
            # off by 1
            if userGuess + 1 == correctNum or userGuess - 1 == correctNum:
                msg = "Off by 1: You are a worthy opponent, Knight. \n"
                msg += "You picked " + str(userGuess) + ", and the actual number was " + str(correctNum) + "\n"
            # off by 2
            elif userGuess + 2 == correctNum or userGuess - 2 == correctNum:
                msg = "Off by 2: You have much to learn, Padawan. \n"
                msg += "You picked " + str(userGuess) + ", and the actual number was " + str(correctNum) + "\n"
            # off by 3
            elif userGuess + 3 == correctNum or userGuess - 3 == correctNum:
                msg = "Off by 3: Youngling, your time will come. \n"
                msg += "You picked " + str(userGuess) + ", and the actual number was " + str(correctNum) + "\n"
            # off by 3+
            else:
                msg = "Off by 3+: Keep working hard in the Service Corps. \n"
                msg += "You picked " + str(userGuess) + ", and the actual number was " + str(correctNum) + "\n"
        # ending message and end game
        if msg == "Exact match: Honored to play with you, Master. \n":
            msg = "Exact match: Honored to play with you, Master. \n"
            msg += "You picked " + str(userGuess) + ", and the actual number was " + str(correctNum) + "\n"
        else:
            msg += "Youngling, your time will come."
    # print message
    print(msg)
    break
