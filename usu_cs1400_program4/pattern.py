# Nate Stott
# CS1400-MO1 XL
# Assignment 4

import turtle
import random

def reset():
    # Erase all of the patterns and start over
    turtle.clear()

def setup():
    # Configure turtle to draw quickly
    turtle.speed(0)
    # Configure turtle to have a window of 1000 x 800
    turtle.setup(1000, 800)

def drawRectanglePattern(centerX, centerY, offset, width, height, count, rotation):
    # Use appropriate parameters
    # See additional information below
    equalPartsOfTheCircle = 360 / count
    heading = 0
    for i in range(1, count + 1):
        turtle.penup()
        turtle.goto(centerX, centerY)
        turtle.setheading(heading)
        turtle.forward(offset)
        turtle.pendown()
        # It should call drawRectangle()
        drawRectangle(width, height, rotation, heading)
        turtle.left(rotation)
        heading += equalPartsOfTheCircle

def drawRectangle(width, height, rotation, heading):
    # Use appropriate parameters
    # set random color
    setRandomColor()
    # Should draw a SINGLE rectangle
    turtle.right(rotation)
    turtle.forward(height)
    turtle.left(90)
    turtle.forward(width)
    turtle.left(90)
    turtle.forward(height)
    turtle.left(90)
    turtle.forward(width)
    turtle.left(90)


def drawCirclePattern(centerX, centerY, offset, radius, count):
    # Use appropriate parameters
    equalPartsOfTheCircle = 360 / count
    heading = 0

    for i in range(1, count + 1):
        turtle.penup()
        turtle.goto(centerX, centerY)
        turtle.setheading(heading)
        turtle.forward(offset)
        turtle.pendown()
        setRandomColor()
        turtle.setheading(270)
        turtle.circle(radius)
        heading += equalPartsOfTheCircle

    # See additional information below

def drawSuperPattern(num):
    # Use appropriate parameters
    # Randomly draw Rectangle and Circle patterns. Each pattern should based on random values.
    # Use reasonable random values (some can be negative) so patterns are drawn on the screen
    count = 0
    while num > count:

        shapeChoice = random.randint(1, 2)
        if shapeChoice == 1:
            drawRectanglePattern(centerX=random.randint(-450, 450), centerY=random.randint(-350, 350),
                                 offset=random.randint(-200, 200), width=random.randint(-200, 200),
                                 height=random.randint(-200, 200), count=random.randint(1, 50), rotation=random.randint(-200, 200))
        elif shapeChoice == 2:
            drawCirclePattern(centerX=random.randint(-450, 450), centerY=random.randint(-350, 350),
                              offset=random.randint(-200, 200), radius=random.randint(-200, 200), count=random.randint(1, 50))
    count += 1


def setRandomColor():
    # Do not use any parameters
    # Set turtle to draw in a random color
    colorChoice = random.randint(1, 4)
    # Use at least 4 colors
    if colorChoice == 1:
        turtle.color("red")
    elif colorChoice == 2:
        turtle.color("blue")
    elif colorChoice == 3:
        turtle.color("black")
    elif colorChoice == 4:
        turtle.color("green")


def done():
    # Called when user quits
    # Keeps the turtle window open
    turtle.hideturtle()
    turtle.done()
