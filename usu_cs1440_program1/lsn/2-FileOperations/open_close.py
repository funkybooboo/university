# Demonstrate how to open and close a file in Python
#
# You may need to interact with the `help` function if it's text output is
# larger than your terminal. Use `Enter` to scroll, and use `q` to exit the
# pager.


# The following two lines show the user their current working directory
from os import getcwd
print(f"Your current location is {getcwd()}")

# Get help with the built in function `open`
print("\n\nPress ENTER to read the help file for open()")
input("Use `Enter` to scroll the help viewer and `q` to exit")
help(open)

# Open *this* exact file.  A File object is stored in the variable `fileObj`
fileObj = open("open_close.py")

# Now, with the opened file object `file`, we can get information on the `close`
# method that can be used on file objects!
help(fileObj.close)

# Close fileObj when you are done with it to not waste computer resources
fileObj.close()
