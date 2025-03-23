# Nate Stott
# CS1400-MO1 XL
# Assignment 6

from deck import Deck
from time import sleep
from gronkyutil import TITLE, GANG

whatAlgorithem = 0

def main():
    print("Welcome to Gronky Cards\n")
    print("Shuffling Cards", end="")
    thinking()

    deck = Deck()
    playerHand = []

    cardCount = int(input("How many cards would you like?: "))

    for i in range(cardCount):
        playerHand.append(deck.draw())

    done = False
    while not done:
        print()
        print("Menu")
        print("\t(1) Display hand")
        print("\t(2) Sort by title")
        print("\t(3) Sort by gang")
        print("\t(4) Search for card")
        print("\t(5) Quit")
        choice = int(input("Choose an option: "))
        print()

        if choice == 1:
            displayHand(playerHand, cardCount)
        elif choice == 2:
            playerHand = sortByTitle(playerHand)
        elif choice == 3:
             playerHand = sortByGang(playerHand)
        elif choice == 4:
            searchForCard(playerHand)
        elif choice == 5:
            done = True

def thinking():
    for i in range(5):
        print(".", end="")
        sleep(0.5)
    print()

def displayHand(hand, cardCount):
    print("Your Hand")
    i = 0
    while i < cardCount:
        print(repr(hand[i]))
        i += 1

def sortCardList(cardList, choice):
    global whatAlgorithem
    if whatAlgorithem == 0:
        print("Selection Sort by " + choice, end="")
        thinking()
        cardList = selectionSort(cardList)
        whatAlgorithem = 1
    elif whatAlgorithem == 1:
        print("Insertion Sort by " + choice, end="")
        thinking()
        cardList = insertionSort(cardList)
        whatAlgorithem = 2
    else:
        print("Bubble Sort by " + choice, end="")
        thinking()
        cardList = bubbleSort(cardList)
        whatAlgorithem = 0
    return cardList

def sortByTitle(hand):
    title = "Title"
    cardList = []
    dictionary = {}

    for i in range(len(hand)):
        card = hand[i]
        cardID = card.getID()
        cardRank = cardID % len(TITLE)

        cardList.append(cardRank)
        dictionary[cardRank] = card

    cardList = sortCardList(cardList, title)
    sortedHand = []
    for i in range(len(cardList)):
        sortedHand.append(dictionary[cardList[i]])

    return sortedHand

def sortByGang(hand):
    gang = "Gang"
    cardList = []
    dictionary = {}

    for i in range(len(hand)):
        card = hand[i]
        cardID = card.getID()
        cardRank = cardID // len(TITLE)

        cardList.append(cardRank)
        dictionary[cardRank] = card

    cardList = sortCardList(cardList, gang)
    sortedHand = []
    for i in range(len(cardList)):
        sortedHand.append(dictionary[cardList[i]])

    return sortedHand

def searchForCard(hand):
    print("Search for Card")
    msg1 = ""
    msg1 += "\t\t (1) One \n"
    msg1 += "\t\t (2) Two \n"
    msg1 += "\t\t (3) Three \n"
    msg1 += "\t\t (4) Four \n"
    msg1 += "\t\t (5) Five \n"
    msg1 += "\t\t (6) Six \n"
    msg1 += "\t\t (7) Seven \n"
    msg1 += "\t\t (8) Eight \n"
    msg1 += "\t\t (9) Nine \n"
    msg1 += "\t\t (10) Ten \n"
    msg1 += "\t\t (11) Baker \n"
    msg1 += "\t\t (12) Jester \n"
    msg1 += "\t\t (13) Page \n"
    msg1 += "\t\t (14) Scribe \n"
    msg1 += "\t\t (15) Squire \n"
    msg1 += "\t\t (16) Armorer \n"
    msg1 += "\t\t (17) Marshal"
    print(msg1)
    inputTitle = input("Choose a Title: ")
    if inputTitle == "1":
        inputTitle = "One"
    elif inputTitle == "2":
        inputTitle = "Two"
    elif inputTitle == "3":
        inputTitle = "Three"
    elif inputTitle == "4":
        inputTitle = "Four"
    elif inputTitle == "5":
        inputTitle = "Five"
    elif inputTitle == "6":
        inputTitle = "Six"
    elif inputTitle == "7":
        inputTitle = "Seven"
    elif inputTitle == "8":
        inputTitle = "Eight"
    elif inputTitle == "9":
        inputTitle = "Nine"
    elif inputTitle == "10":
        inputTitle = "Ten"
    elif inputTitle == "11":
        inputTitle = "Baker"
    elif inputTitle == "12":
        inputTitle = "Jester"
    elif inputTitle == "13":
        inputTitle = "Page"
    elif inputTitle == "14":
        inputTitle = "Scribe"
    elif inputTitle == "15":
        inputTitle = "Squire"
    elif inputTitle == "16":
        inputTitle = "Armorer"
    elif inputTitle == "17":
        inputTitle = "Marshal"

    msg2 = ""
    msg2 += "\t\t (1) Jets \n"
    msg2 += "\t\t (2) Pollos \n"
    msg2 += "\t\t (3) Slugs \n"
    msg2 += "\t\t (4) Yokels \n"
    msg2 += "\t\t (5) Keiths \n"
    msg2 += "\t\t (6) Elbows"
    print(msg2)
    inputGang = input("Choose a Gang: ")
    if inputGang == "1":
        inputGang = "Jets"
    elif inputGang == "2":
        inputGang = "Pollos"
    elif inputGang == "3":
        inputGang = "Slugs"
    elif inputGang == "4":
        inputGang = "Yokels"
    elif inputGang == "5":
        inputGang = "Keiths"
    elif inputGang == "6":
        inputGang = "Elbows"

    sortByGang(hand)

    print("Binary Search for " + inputTitle + " of " + inputGang, end="")
    thinking()

    gotIt = 0
    for i in range(len(hand)):
        card = hand[i]
        if card.getTitle() == inputTitle and card.getGang() == inputGang:
            print("\t\tCongrats! You have that card")
            gotIt = 1
            break
    if gotIt == 0:
        print("\t\tSorry. You do not have that card")

def selectionSort(listOfRank):
    for i in range(len(listOfRank) - 1):
        currMinIndex = i
        for j in range(i + 1, len(listOfRank)):
            if listOfRank[currMinIndex] > listOfRank[j]:
                currMinIndex = j
        if currMinIndex != i:
            listOfRank[i], listOfRank[currMinIndex] = listOfRank[currMinIndex], listOfRank[i]
    return listOfRank

def insertionSort(listOfRank):
    for i in range(1, len(listOfRank)):
        currElement = listOfRank[i]
        j = i - 1
        while j >= 0 and listOfRank[j] > currElement:
            listOfRank[j + 1] = listOfRank[j]
            j -= 1
        listOfRank[j + 1] = currElement
    return listOfRank

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
