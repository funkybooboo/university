import os
# We import sys for the function sys.exit to exit the program at any given point
import sys


def getFileSafely(path):
    '''
    Function to safely open a file object. If `path` can be accessed, open the
      file located at `path` and return it. If `path` does not exist or cannot
      be `access`ed, the program exits by calling `sys.exit(1)` after warning
      the user that `path` is not accessible.
    '''
    if os.access(path, os.R_OK):
        return open(path)
    else:
        print("The file provided could not be accessed.")
        sys.exit(1)

if __name__ == '__main__':
    cwd = os.getcwd()
    print(f"Please enter a file path relative to {cwd}")
    filename = input("File Path: ")
    file = getFileSafely(filename)
    # The following line should *NEVER* get executed if `filename` is invalid
    print("Congratulations! You've specified a file that exists!")
    file.close()
