# The `lsn` Directory
These guided lessons will be provided *only* for this assignment here at DuckieCorp. Completion of these lessons and their exercises **will be graded**.

## 3 Lesson Directories And Lesson Completion
Included in this `lsn` directory are 3 directories containing different lessons. These directories are:

* [`0-ASCIIChars`](0-ASCIIChars)
* [`1-TextProcessing`](1-TextProcessing)
* [`2-FileOperations`](2-FileOperations)

Each lesson directory consists of
*   a `README.md` file explaining the lesson and your tasks for its exercises.
    * There may be other helpful `*.md` files too! Don't ignore these as they'll teach important concepts needed for the lessons.
*   `ex*.py` files to be completed according to the specifications in `README.md`.
*   `runTests.py` a test script to run to verify completion of the associated lesson and it's exercises.

To complete these three lessons, the `runTests.py` script must *pass* for every exercise present in each of the 3 lesson directories.

## Lesson README.md
There will be one `README.md` file present in each lesson directory. This file will be your guide through the associated lesson. This will also tell what must be done to complete each exercise associated with the lesson. Read each section and any linked documents to get the most out of each lesson!

**Lesson tip:** while reading the `README.md`, you will find it beneficial to *read all sub-sections following an exercise before starting an exercise.* For example, when reading the `README.md` for lesson 0, and starting to work on it's first exercise, one will want to read the section for exercise 0, f-strings and their examples, and `ex0.py`'s test conditions (stopping at the exercise 1 section) *before* starting work on exercise 0. Valuable information is found in each of these sections that you do not want to miss!

## Verifying Your Work By Running `runTests.py`
Each lesson has an associated suite of unit tests that have been provided. These tests belong in `$LSN_DIR/tests`, and they should **not be modified**. Instead, a `runTests.py` script is provided to invoke these tests for you and provide more information regarding these tests. There is no need to worry *how* these tests are done at this point in your internship: your only concern is to invoke these tests and complete the exercises such that they pass the tests.

To invoke the most basic running of the `runTests.py` script for a given lesson, follow the provided instructions:
*   `cd` into the given lesson directory.
*   Launch Python with the `runTests.py` file to run all of the tests in the lesson.
    *   EX: From within `0-ASCIIChars`, running `python runTests.py` **will produce correct output** for all exercises associated with lesson 0.
*   **NOTE: The current working directory *must* be the associated lesson directory for the `runTests.py` to work correctly.**
    *   EX: From the root of the project, running `python lsn/2*/runTests.py` **will produce incorrect output**.

**Example**

```
$ cd 0-ASCIIChars
$ python runTests.py

Welcome to the DuckieCorp's automated testing system! Verify if you have
successfully completed the requested lesson exercises.
You have requested tests on the following exercises:
    ASCII Characters Exercise 0
    ASCII Characters Exercise 1
    ASCII Characters Exercise 2
    ASCII Characters Exercise 3

[Wall of text goes here]

```

A lot of text will be printed.  Don't be scared, it's just trying to help!

Start from the top, complete each lesson, and work your way down to the bottom.  As you go you will turn the scary red errors into pleasant green compliments.

You can focus on just one exercise at a time by giving `runTests.py` extra arguments:


### Run all tests
```
$ python runTests.py
```

### Test only exercises 1 and 3
```
$ python runTests.py 1 3
```

### Printing the `runTests.py` usage message
```
$ python runTests.py -h
USAGE: $ python runTests.py [TESTS]

If [TESTS] is ommitted, the program will run all provided tests against the
exercises.

[TESTS] is an optional parameter of space separated integers. This is
provided only if the user wants to run specific tests. To run only tests
against exercises 0 and 3, one would input `python runTests.py 0 3`.

The following tests can be run:
    0 : ASCII Characters Exercise 0
    1 : ASCII Characters Exercise 1
    2 : ASCII Characters Exercise 2
    3 : ASCII Characters Exercise 3
```

## Running Individual Exercises
Each exercise has it's own `ex*.py` file. This file will have function(s) that are to be completed by you according to the lesson's `README.md` specifications. To manually test these functions, you can invoke these `ex*.py` files directly, which will invoke the code found in the `if __name__ == '__main__':` section. Code belonging to this section will only execute if the file is *directly invoked as the main file.* None of this code will execute if this module is imported. Feel free to modify any of the code in the section for your own tests, as code in the `if __name__ == "__main__":` section will not effect the automated tests.

* EX:
    * In the lesson 0 directory, run `python ex0.py` to invoke the created `displayASCII()` function for your own convenience!
    * In the lesson 0 directory, run `python ex1.py` to test the created `listOfChars` function against a provided data set. If `listOfChars` is done correctly--assuming the provided code in the `if __name__ == '__main__':` section was not modified--it will produce the string `"A short sentence."` to the console.

### Running Individual Exercises in Interactive Mode
Running `python -i [file]` will load the contents of the given file *then* put you immediately into the Python REPL. This can be incredibly handy while crafting and debugging some of these functions for each exercise.

* EX:
    * In the lesson 0 directory, run `python -i ex1.py`. This will give you access to your current implementation of the `listOfChars()` function in the REPL! You can then directly try putting in your own list of integers for specific testing.