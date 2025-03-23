import sys

def helper():
    print("To use program enter \"python3 src/tt.py TOOL [OPTIONS] FILENAME...\"")
    print()
    print("\t\"python3\" to call the python interpreter")
    print()
    print("\t\"src/tt.py\" to start the the program")
    print()
    print("\t\"TOOL\" so that the program knows what you want to do with the FILENAME you enter")
    print()
    print("\t\"[OPTIONS]\" is only for specific programs you can type \"python3 src/tt.py help\" for more information")
    print()
    print("\t\"FILENAME\" you need to enter the path to a file so that the entered TOOL can do something with that file \n\tYou can enter many files seperated by a space")
    print()
    sys.exit(2)