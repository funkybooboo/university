# CS 1440 Assignment 1: DuckieDecrypter - The DuckieEncrypter

In the `demo` directory you will find `duckieEncrypter.py`, a program that creates DuckieCrypt from plain text.  This is the companion program to the decrypter that you have been asked to write.

This program is *not* part of the assignment, you do *not* need to change it, and it will *not* be looked at during code review.  It is provided to help you better understand the DuckieCrypt encryption scheme.


## Usage

When no arguments are given, this program asks the user for some text to encrypt.  When a filename is given on the command-line, DuckieEncrypter opens and reads the text from the file, *encrypts* it, and prints DuckieCrypt to the screen.  The file named on the command-line is left unchanged.

```
USAGE: $ python demo/duckieEncrypter.py [[FILE]]

[FILE] is an optional argument that is the path to a text file to encrypt.

If [FILE] is not given, the user is prompted to enter a single line of text
input to encrypt.  
```

From the root of the project run `$ python demo/duckieEncrypter.py` to encrypt a
single line of text.

From the root of the project run `$ python demo/duckieEncrypter.py
data/encryptMe1.txt` to encrypt the file `data/encryptMe1.txt`.
