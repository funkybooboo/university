# Nate Stott
# CS1400-MO1 XL
# Assignment 2B

import turtle

# Body
turtle.penup()
turtle.goto(0, -200)
turtle.pendown()
turtle.width(2)
turtle.color("black")
turtle.fillcolor("white")
turtle.begin_fill()
turtle.circle(100)
turtle.end_fill()

turtle.penup()
turtle.goto(0,-25)
turtle.pendown()
turtle.begin_fill()
turtle.circle(75)
turtle.end_fill()

turtle.penup()
turtle.goto(0, 100)
turtle.pendown()
turtle.begin_fill()
turtle.circle(50)
turtle.end_fill()

# Hat
turtle.color("green", "purple")
turtle.width(3)
turtle.penup()
turtle.goto(-65, 175)
turtle.pendown()
turtle.begin_fill()
turtle.setheading(360)
turtle.forward(125)
turtle.setheading(90)
turtle.forward(10)
turtle.setheading(180)
turtle.forward(20)
turtle.setheading(60)
turtle.forward(45)
turtle.setheading(180)
turtle.forward(130)
turtle.setheading(300)
turtle.forward(45)
turtle.setheading(180)
turtle.forward(20)
turtle.setheading(270)
turtle.forward(10)
turtle.end_fill()


# Right Eye
turtle.color("black")
turtle.penup()
turtle.goto(20, 160)
turtle.pendown()
turtle.begin_fill()
turtle.circle(5)
turtle.end_fill()

# Left Eye
turtle.penup()
turtle.goto(-20, 160)
turtle.pendown()
turtle.begin_fill()
turtle.circle(5)
turtle.end_fill()

# Mouth
turtle.color("red")
turtle.penup()
turtle.goto(-25, 130)
turtle.pendown()
turtle.width(5)
turtle.setheading(315)
turtle.circle(35, 90)

# Buttons
turtle.width(2)
turtle.color("blue")
turtle.penup()
turtle.goto(0, 25)
turtle.pendown()
turtle.begin_fill()
turtle.circle(5)
turtle.end_fill()

turtle.penup()
turtle.goto(0, 40)
turtle.pendown()
turtle.begin_fill()
turtle.circle(5)
turtle.end_fill()

turtle.penup()
turtle.goto(0, 55)
turtle.pendown()
turtle.begin_fill()
turtle.circle(5)
turtle.end_fill()

# Right Arm
turtle.width(5)
turtle.color("brown")
turtle.penup()
turtle.goto(65, 50)
turtle.pendown()
turtle.setheading(45)
turtle.forward(75)
turtle.penup()
turtle.backward(15)
turtle.pendown()
turtle.setheading(90)
turtle.forward(15)
turtle.penup()
turtle.backward(15)
turtle.pendown()
turtle.setheading(360)
turtle.forward(15)

# Left Arm
turtle.penup()
turtle.goto(-65, 50)
turtle.pendown()
turtle.setheading(220)
turtle.forward(75)
turtle.penup()
turtle.backward(15)
turtle.pendown()
turtle.setheading(180)
turtle.forward(15)
turtle.penup()
turtle.backward(15)
turtle.pendown()
turtle.setheading(270)
turtle.forward(15)


# Finishing up
turtle.hideturtle()
turtle.done()
