# Nate Stott
# CS1400-MO1 XL
# Assignment 7

from modules.memoryboard import MemoryBoard

# This import gives some funcitonality if your run the program from the commandline. See the video in the assignment
# for instructions. Running in the terminal makes the game board play more like a game instead of scrolling and
# printing a new board every time.
# You'll need to comment a line down below in the printGameBoard() function if using PyCharm

# You must identify and fix bugs you find in the file.
# Save these in a file called memory_bugs.txt. It should look something like this.
# Note that these may or may not be actual bugs in the program.
# The number of bugs is unknown (to you).
# A bug is anything that makes the gameplay experience not match expectations.

# 1. The game starts with a welcome message
# 2. The user is prompted to enter the number of columns on the game board
# 3. The user is prompted to enter the number of rows on the game board
# 4. The user is prompted to enter the number of players, which will be named Player 1, Player 2, etc.
# Game loop
    # 5. Prior to each prompting during game play, the score for each player is printed followed by the game board (see below)
    # 6. The current player is prompted to input an x, y coordinate
    # 7. If the user ever enters the coordinates of a card that is face-up already, they are notified of the error and need to try again
    # 8. If the spot is valid the card is flipped face-up.
    # 9. Process repeats for a second card
    # 10. If a match is detected the player scores a point and goes again
    # 11. If a match is not detected the two cards are flipped face-down and the next player goes
    # 12. This continues until all possible matches are made
# After all matches are made, one of two possible messages are displayed
    # 13. If it is a tie, then it is so stated along with an indication of the players who tied
    # 14. If there is a single winner then it is so stated along with a message of who the winner is


def main():
    playerTurn = 0  # Keep track of whose turn it is
    theMatchsThatHaveBeenMade = []
    print("Welcome to the Memory Game\n")

    # Get the Board Size
    cols = int(input("Enter the number of columns on the board: "))
    rows = int(input("Enter the number of rows on the board: "))

    # Get the number of players
    playerCount = int(input("Enter the number of players: "))
    scores = [0] * playerCount

    # Create the MemoryBoard object
    memoryBoard = MemoryBoard(rows, cols)


    # needed to make a verible for how many matchs there can be BUG
    howManyMatchsThereCanBe = (cols * rows)
    if howManyMatchsThereCanBe % 2 == 1:
        howManyMatchsThereCanBe -= 1
    howManyMatchsThereCanBe /= 2

    # Game loop
    winner = False
    while not winner:


        selectedCards = [] # Track the cards the current player selected
        printGameBoard(scores, memoryBoard.getBoard())
        # Each turn is two card flips
        while len(selectedCards) < 2:
            xPos, yPos = eval(input("Player " + str(playerTurn + 1) + " choose a card to flip: "))
            # needed to check if the card was already flipped for the second move BUG
            # needed to check if the card was already flipped for cards that already had matches BUG
            if ((len(selectedCards) == 1 and selectedCards[0][0] == xPos and selectedCards[0][1] == yPos)
                    or memoryBoard.isCardFlipped(xPos, yPos)):
                input("That card has been flipped. Hit Enter to try again.")
            else:
                # need to store move one and two BUG
                selectedCards.append([xPos, yPos])
                memoryBoard.flipCard(xPos, yPos)
            printGameBoard(scores, memoryBoard.getBoard())

            # Player goes again if they get a match
            # needed to stop the game when all the posible matches have been made BUG
        if len(theMatchsThatHaveBeenMade) >= howManyMatchsThereCanBe:
            input("Hit Enter to Continue")
            break
        elif memoryBoard.isMatch(selectedCards[0], selectedCards[1]):
            print("You got a match!")
            # playerTurn is 0 based BUG
            scores[playerTurn] += 1
            winner = sum(scores) == cols * rows / 2
            theMatchsThatHaveBeenMade.append(selectedCards)

            # If the game is over show a different message
            # needed to stop the game when all the posible matches have been made for if they got a match BUG
            if len(theMatchsThatHaveBeenMade) >= howManyMatchsThereCanBe:
                input("Hit Enter to Continue")
                break
            elif winner:
                # changed print to input BUG
                input("Hit Enter to Continue")
            else:
                input("Player " + str(playerTurn + 1) + " hit Enter to go again.")
        else:
            # needed to change the coordinance for the list BUG
            memoryBoard.flipCard(selectedCards[0][0], selectedCards[0][1])
            memoryBoard.flipCard(selectedCards[1][0], selectedCards[1][1])
            print("No Match")
            # needed to change when player turn is incrimentmed BUG
            playerTurn += 1
            # needed to reset when playerTurn is set BUG
            if playerTurn >= playerCount:
                playerTurn = 0
            input("Hit Enter for Player " + str(playerTurn + 1))




    # Find the winning score value
    highScore = max(scores)

    # added a print so it doesnt hit enter for me BUG
    print()

    # Check for a tie/winner
    if scores.count(highScore) > 1:
        print("The following players tied: ", end="")
        for i in range(len(scores)):
            if scores[i] == highScore:
                # changed i + 1 to i + 2 beucse it was printing 0 1 and not 1 2 BUG
                print(i + 1, end=" ")
    else:
        print("The winner is player " + str(scores.index(highScore) + 1))


# Print the scoreboard and gameboard
def printGameBoard(scores, board):
    #os.system('cls||clear') # Comment this out if running in PyCharm
    #added a indent so it looks better BUG
    print("\n\n\n\n\n\n\n\n\n\n\n\n\n")
    for i in range(len(scores)):

        print("Player " + str(i + 1) + ": " + str(scores[i]))
    print()
    print(board)


main()
