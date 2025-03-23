# Nate Stott
# CS1400-MO1 XL
# Assignment 4

from chessboard import drawChessboard


def main():
    userStartPosition = input("Enter starting Positions (Ex- 100, 100): ")
    width = input("Enter the Width: ")
    height = input("Enter the Height: ")

    # split positions
    userStartPositionSplit = userStartPosition.split(",")
    startX = eval(userStartPositionSplit[0].strip())
    startY = eval(userStartPositionSplit[1].strip())

    if width == "" and height == "":
        drawChessboard(startX, startY)
    elif height == "":
        drawChessboard(startX, startY, width=eval(width))
    elif width == "":
        drawChessboard(startX, startY, height=eval(height))
    else:
        drawChessboard(startX, startY, eval(width), eval(height))


main()
