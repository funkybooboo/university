# Nate Stott
# CS1400-MO1 XL
# Assignment 5

import turtle

class Face:
    def __init__(self):
        self.__smile = True
        self.__happy = True
        self.__darkEyes = True

    def draw_face(self):
        turtle.clear()
        self.__drawHead()
        self.__drawEyes()
        self.__drawMouth()

    def isSmile(self):
        return self.__smile

    def isHappy(self):
        return self.__happy

    def isDarkEyes(self):
        return self.__darkEyes

    def changeMouth(self):
        self.__smile = not self.__smile
        self.draw_face()

    def changeEmotion(self):
        self.__happy = not self.__happy
        self.draw_face()

    def changeEyes(self):
        self.__darkEyes = not self.__darkEyes
        self.draw_face()

    def __drawHead(self):
        # set face color
        if self.__happy == True:
            turtle.fillcolor("Yellow")
        else:
            turtle.fillcolor("Red")
        # draw face
        turtle.showturtle()
        turtle.penup()
        turtle.goto(0, -100)
        turtle.pendown()
        turtle.begin_fill()
        turtle.setheading(0)
        turtle.circle(100)
        turtle.end_fill()

    def __drawEyes(self):
        # set eye color
        if self.__darkEyes == True:
            turtle.fillcolor("Black")
        else:
            turtle.fillcolor("Blue")
        # left eye
        turtle.penup()
        turtle.goto(-25, 40)
        turtle.pendown()
        turtle.begin_fill()
        turtle.circle(10)
        turtle.end_fill()
        # right eye
        turtle.penup()
        turtle.goto(25, 40)
        turtle.pendown()
        turtle.begin_fill()
        turtle.circle(10)
        turtle.end_fill()

    def __drawMouth(self):
        # if smile
        turtle.penup()
        if self.__smile == True:
            heading = 310
            turtle.goto(-45, -40)
        # if froun
        else:
            heading = 130
            turtle.goto(45, -40)
        # draw mouth
        turtle.pendown()
        turtle.width(5)
        turtle.setheading(heading)
        turtle.circle(60, 100)
        turtle.width(1)
        turtle.hideturtle()

def main():
    face = Face()
    face.draw_face()

    done = False

    while not done:
        print("Change My Face")
        mouth = "frown" if face.isSmile() else "smile"
        emotion = "angry" if face.isHappy() else "happy"
        eyes = "blue" if face.isDarkEyes() else "black"
        print("1) Make me", mouth)
        print("2) Make me", emotion)
        print("3) Make my eyes", eyes)
        print("0) Quit")

        menu = eval(input("Enter a selection: "))

        if menu == 1:
            face.changeMouth()
        elif menu == 2:
            face.changeEmotion()
        elif menu == 3:
            face.changeEyes()
        else:
            break

    print("Thanks for Playing")

    turtle.hideturtle()
    turtle.done()


main()
