# Nate Stott
# CS1400-MO1 XL
# Assignment 3

from random import randint

runSim = True
# start sim
while runSim:
    # make varables to play around with later
    numOfTimesZooKeeperFindsAnEle = 0
    numOfTimesZooKeeperFindsEleTogether = 0
    numOfInterations = 100000

    # start for loop to start the tests
    for count in range(numOfInterations):

        # get picks from all the chariters
        elephant1PinChoice = randint(1, 6)
        elephant2PinChoice = randint(1, 6)
        zooKeeperPenChoice = randint(1, 6)

        # tests for if the zoo keeper found the elephants together or alone
        if elephant1PinChoice == elephant2PinChoice and elephant1PinChoice == zooKeeperPenChoice:
            numOfTimesZooKeeperFindsEleTogether += 1
        elif elephant1PinChoice == zooKeeperPenChoice or elephant2PinChoice == zooKeeperPenChoice:
            numOfTimesZooKeeperFindsAnEle += 1

    # calculate percenteges
    percentNumOfTimeEleWasFound = (float(numOfTimesZooKeeperFindsAnEle) / numOfInterations) * 100
    percentNumOfTimeEleWasFoundTogether = (float(numOfTimesZooKeeperFindsEleTogether / numOfInterations)) * 100

    # print out information to user
    print("The Zoo Keeper found an elephant " + '{:.2f}'.format(percentNumOfTimeEleWasFound) + "% of the time")
    print("The Zoo Keeper found the elephants together " + '{:.2f}'.format(percentNumOfTimeEleWasFoundTogether) + "% of the time")

    # check if user wants to run sim again
    userOption = input("Run the simulation agian? (y or n): ")
    if userOption == "n":
        runSim = False
