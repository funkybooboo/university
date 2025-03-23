# Nate Stott
# CS1400-MO1 XL
# Assignment 2

import turtle

# get user data
print("Make a custem target!")
print("Example (X, Y)")
xPos = int(input("First Enter a X Value: "))
yPos = int(input("Second Enter a Y Value: "))
diameter = int(input("Third Enter a Diameter Value: "))
print("Alright lets print!")

# calculate
radius = diameter / 2

# Fourth layer
turtle.penup()
turtle.color("black")
yPos = (radius * -4)
turtle.goto(xPos, yPos)
turtle.pendown()
turtle.fillcolor("black")
turtle.begin_fill()
turtle.circle(radius * 4)
turtle.end_fill()

# Third layer
turtle.penup()
turtle.color("light blue")
yPos = (radius * -3)
turtle.goto(xPos, yPos)
turtle.fillcolor("light blue")
turtle.begin_fill()
turtle.circle(radius * 3)
turtle.end_fill()

# Second layer
turtle.penup()
turtle.color("red")
yPos = (radius * -2)
turtle.goto(xPos, yPos)
turtle.pendown()
turtle.fillcolor("red")
turtle.begin_fill()
turtle.circle(radius * 2)
turtle.end_fill()

# Bullseye
turtle.penup()
turtle.color("yellow")
yPos = (radius * -1)
turtle.goto(xPos, yPos)
turtle.pendown()
turtle.fillcolor("yellow")
turtle.begin_fill()
turtle.circle(radius)
turtle.end_fill()

# Finishing up
turtle.hideturtle()
turtle.done()
