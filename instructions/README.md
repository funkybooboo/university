# CS 1440 Assignment 1: DuckieDecrypter - Instructions

## Table Of Contents
[TOC]

## `instructions` Directory

This directory contains instructor-provided documentation files.

*   `README.md` - The requirements of the assignment
*   `Rubric.md` - What each requirement is worth to your grade.  The same
    information as is found on Canvas.
*   `Output.md` - The expected program output under various circumstances.
*   `Hints.md` -  Miscellaneous time-saving ideas.  Scan this when you get stuck.
*   `UnderstandingDuckieCrypt.md` - A guide to understanding the DuckieCrypt
    encryption algorithm.

Please leave this directory as-is in case the instructor needs to send out corrections.

Your documentation belongs in `doc/`.


## Description

Your ultimate goal is to create the DuckieDecrypter, a tool to *decrypt*  messages containing DuckieCrypt into plain English. This program decrypts data encoded in DuckieCrypt from a user-specified text file, and is detailed [below](#behavior-of-the-duckiedecrypter-program). You have been given a working DuckieCrypt *encrypter*; a program that turns plain text into DuckieCrypt for your own testing purposes. It is located in [`demo/duckieEncrypter.py`](../demo/duckieEncrypter.py) and does not need to be modified in any way.

One of our previous interns started this project but abandoned it in the early stages of the implementation phase. They have left behind some starter code located in [`src/duckieDecrypter.py`](../src/duckieDecrypter.py) that has some fully fleshed out helper functions *and* some "function stubs" that might help you understand the way *they* wanted to approach this problem. You are not required to follow their design that is laid out in this starter code. The only thing that *cannot* be changed in the starter code will be explicitly marked with a comment from management.

To help you have more success we have also created lessons and associated exercises to assist you in learning the Python skills necessary to complete this project, which are detailed more in depth [below](#guided-lessons). All interns must complete these lessons and their associated exercises. 

## Previous Semester Assignment Statistics

Statistic                        | Value
--------------------------------:|:---------------
Average Hours Spent              | 7.87
Average Score % (Grade)          | 87.8% (B+)
% students thought this was Easy | 32.5%
... Medium                       | 56.1%
... Hard                         | 7.3%
... Too Hard/Did not complete    | 4.1%

## Guided lessons

Under the [`lsn`](../lsn) directory are three lessons designed to assist you in learning the necessary skills to create the DuckieDecrypter. These lessons will only be provided for this DuckieDecrypter task for our interns. Do not expect them to be present for future tasks.

Each lesson consists of:
*   a `README.md` file explaining the lesson and your tasks for its exercises.
    * There may be other helpful `*.md` files too!
*   `ex*.py` files to be completed according to the specifications in `README.md`.
*   `runTests.py` which is run to verify completion of the associated lesson.

These lessons will be assessed according to the completion of the exercise and the passage of the associated unit tests. View [the README in the `lsn` directory](../lsn) for more info on running the `runTests.py` file from each `lsn` subdirectory. **These lessons and their associated exercises will be graded.** The software development plan is not required for these lessons and exercises.

### Topics of the lessons

0.  Start in [`0-ASCIIChars/`](lsn/0-ASCIIChars) to learn about ASCII characters and ordinal values.
1.  Next, head into [`1-TextProcessing/`](lsn/1-TextProcessing) to learn how to process text line by line and word by word in Python.
2.  Finally, [`2-FileOperations/`](lsn/2-FileOperations) will teach you how you can open, read and close files in Python.

## Understanding DuckieCrypt

DuckieCrypt is an [encoding](https://en.wikipedia.org/wiki/Character_encoding) of the first 95 printable ASCII characters. It allows one to translate plain-text messages into text that cannot be read by those who are unaware of it's encoding. One can translate the character `A` into the DuckieCrypt character `^0`.

More details on the structure of DuckieCrypt and it's character-by-character translation is detailed within the [`UnderstandingDuckieCrypt.md` file](UnderstandingDuckieCrypt.md). Understanding DuckieCrypt well is key to creating a tool that allows one to decrypt a message from DuckieCrypt back to plain English.

## Behavior Of The DuckieDecrypter Program

Your DuckieDecrypter will be provided with a path to a text file that may contain DuckieCrypt characters. Valid DuckieCrypt characters contained in this text file are to be decrypted back to plain text and the resultant message should be printed. 

When reading a file and translating it into plain text, newlines that exist in the original file are to be preserved. 

For example, if a file were to contain the contents
```
^0
^0 ^1
^0 ^1 ^2
```

Your DuckieDecrypter should output
```
A
AB
ABC
```

### Running the DuckieDecrypter program

When a user runs `duckieDecrypter.py` they are *prompted* for a **relative path** to a file to decrypt. **Relative paths** are discussed in `lsn/2-FileOperations`.
 
```
$ python src/duckieDecrypter.py
Please select a text file to decrypt:
>>> data/msg0.txt
Welcome to DuckieCorp! We sure are glad to have you working with us. In your
tenure here, we hope you will learn many new techniques to enhance your computer
science and problem solving skills. We're excited to get started!
- DuckieCorp Management
```

Please note that `$` denotes a command in the shell, and `>>>` denotes input in Python.

There also exists a way for the user to provide *command line arguments* to the program. This is handled by the starter code for the purpose of assisting management in testing your final product. There is also a help message if the user gives a command line argument of `-h` or `--help`. 

So long as you don't modify the parts of the starter code that have comments from management asking you not to modify it, *these features will be given to you for free.*

### Invalid Files Requested

When the user provides a path to an *invalid* file path, your program should safely exit and provide a message noting the program is quitting with a reason. The recommended process of determining if a file path is valid or invalid is found within the [lessons](#guided-lessons). You may find a function that will help to `sendError` messages while exiting the program in the starter code provided by our previous intern.

### Invalid DuckieCrypt Characters

When your program reads a text file and attempts to decrypt text that is *not* a valid DuckieCrypt character, it should **continue along and produce *no output* in it's place in the final message.** When a message is provided with no valid DuckieCrypt characters, the decrypter will output no text; only the preserved new line characters. Valid DuckieCrypt characters contained in a message with invalid DuckieCrypt characters should still be decrypted correctly and in order. 

To illustrate this, consider this message in DuckieCrypt.
```
^7 _4 _11 _11 _14 +A0 ^22 _14 _17 _11 _3 +A1
```
If one were to run this through the DuckieDecrypter, they would find this message outputs `Hello World!`.

If one were to modify the DuckieCrypt message to be:
```
NOTACHAR ^-3000 ^7 _4 _11 _11 SOMESTUFF HERE _14 +A0 ^22 _14 _17 _11 ^TRICKY _3 +A1 1337ELITE1337
```

Running it through the DuckieDecrypter would still produce the same output it did before; `Hello World!`.

#### Invalid Character Tests
In the `data/test` directory, you will find data sets of invalid characters in files named `invalid*`. Your program is required to handle all of these files correctly. If your program crashes while attempting to read non-text files or files containing deceptively formatted almost-DuckieCrypt characters--but handles the ones in `data/test/invalid*` fine--it will **not** be held against you. 

