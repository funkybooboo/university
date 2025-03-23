# Nate Stott
# CS1400-MO1 XL
# Assignment 4

import turtle

def drawChessboard(startX=0, startY=0, width=250, height=250):

    # get ready
    turtle.penup()
    turtle.goto(startX, startY)
    turtle.pendown()
    turtle.color("brown")

    # draw box
    turtle.setheading(0)
    turtle.forward(width)
    turtle.setheading(90)
    turtle.forward(height)
    turtle.setheading(180)
    turtle.forward(width)
    turtle.setheading(270)
    turtle.forward(height)

    # start drawing all the rectangles of the board
    counter = 1
    turtle.color("black")

    turtle.penup()
    turtle.goto(startX, startY)
    turtle.pendown()

    drawAllRectangles(counter, startX, startY, width, height)


def drawAllRectangles(counter, startX, startY, width, height):
    count = 1

    # finds the size of the rectangle
    leftNRightForward = width * (1 / 8)
    upNDownForward = height * (1 / 8)

    # draw each rectangle
    drawRectangle(counter, count, startX, startY, width, height, leftNRightForward, upNDownForward)


def drawRectangle(counter, count, startX, startY, width, height, leftNRightForward, upNDownForward):

    # draws the little black rectangle
    turtle.fillcolor("black")
    turtle.begin_fill()
    turtle.setheading(0)
    turtle.forward(leftNRightForward)
    turtle.setheading(90)
    turtle.forward(upNDownForward)
    turtle.setheading(180)
    turtle.forward(leftNRightForward)
    turtle.setheading(270)
    turtle.forward(upNDownForward)
    turtle.end_fill()

    # if layer one and two are complete then done
    if counter == 3:
        # Ending
        turtle.hideturtle()
        turtle.done()

    # test if layer is done
    if count % 4 == 0:
        count += 1
        turtle.penup()
        turtle.setheading(90)
        turtle.forward(upNDownForward)
        turtle.forward(upNDownForward)
        turtle.setheading(180)
        turtle.forward(leftNRightForward * 6)
        turtle.setheading(0)

        # once all the rows are done
        if count == 17:
            turtle.penup()
            turtle.setheading(0)
            turtle.forward(leftNRightForward)
            turtle.setheading(270)
            turtle.forward(upNDownForward * 7)
            turtle.pendown()
            # calls function to move onto the next layer
            counter += 1
            drawAllRectangles(counter, startX, startY, width, height)
        else:
            drawRectangle(counter, count, startX, startY, width, height, leftNRightForward, upNDownForward)


    # calls function that moves the turtle over one space to the left
    skipASpace(counter, count, startX, startY, width, height, leftNRightForward, upNDownForward)


def skipASpace(counter, count, startX, startY, width, height, leftNRightForward, upNDownForward):


    if count < 16:
        count += 1

        # move turtle over so theres a white space
        turtle.penup()
        turtle.setheading(0)
        turtle.forward(leftNRightForward)
        turtle.forward(leftNRightForward)
        turtle.pendown()

    # draw next square

    drawRectangle(counter, count, startX, startY, width, height, leftNRightForward, upNDownForward)
