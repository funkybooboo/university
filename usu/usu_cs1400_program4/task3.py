# Nate Stott
# CS1400-MO1 XL
# Assignment 4

import pattern

def main():

    # Play again loop
    playAgain = True

    while playAgain:
        # Present a menu to the user
        # Let them select mode
        print("Choose a mode")
        print("1) Rectangle Pattern")
        print("2) Circle Pattern")
        print("3) Super Pattern")
        mode = eval(input("Which mode do you want to play? 1, 2 or 3: "))

        # If they choose 'Rectangle Patterns'
        if mode == 1:
            #### Add Input Statement(s) as needed ####
            userStartPosition = input("Center point ie(100, 100): ")
            userStartPositionSplit = userStartPosition.split(",")
            centerX = eval(userStartPositionSplit[0].strip())
            centerY = eval(userStartPositionSplit[1].strip())
            offset = int(input("Offset: "))
            width = int(input("Width: "))
            height = int(input("Height: "))
            count = int(input("Count: "))
            rotation = int(input("Rotation: "))
            #### End Add Inputs Statement(s) ####

            # Setup pattern
            pattern.setup()

            # Draw the rectangle pattern
            pattern.drawRectanglePattern(centerX, centerY, offset, width, height, count, rotation)

        # If they choose 'Circle Patterns'
        elif mode == 2:
            #### Add Input Statement(s) as needed ####
            userStartPosition = input("Center point ie(100, 100): ")
            userStartPositionSplit = userStartPosition.split(",")
            centerX = eval(userStartPositionSplit[0].strip())
            centerY = eval(userStartPositionSplit[1].strip())
            offset = int(input("Offset: "))
            radius = int(input("Radius: "))
            count = int(input("Count: "))
            #### End Add Inputs Statement(s) ####

            # Setup pattern
            pattern.setup()

            # Draw the circle pattern
            pattern.drawCirclePattern(centerX, centerY, offset, radius, count)

        # If they choose 'Super Patterns'
        elif mode == 3:
            #### Add Input Statement(s) as needed ####
            num = input("Enter number: ")
            #### End Add Inputs Statement(s) ####
            # Setup pattern
            pattern.setup()
            if num == "":
                pattern.drawSuperPattern(num=10)
            else:
                pattern.drawSuperPattern(int(num))

        # Play again?
        print("Do you want to play again?")
        print("1) Yes, and keep drawings")
        print("2) Yes, and clear drawings")
        print("3) No, I am all done")
        response = eval(input("Choose 1, 2, or 3: "))

        #### Add Statement(s) to clear drawings and play again ####
        if response == 1:
            continue
        elif response == 2:
            pattern.reset()
            continue
        elif response == 3:
            break


        #### End Add Inputs Statement(s) ####

    # print a message saying thank you
    print("Thanks for playing!")
    pattern.done()


main()
