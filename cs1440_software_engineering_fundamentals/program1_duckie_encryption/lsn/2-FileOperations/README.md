# Opening And Closing Files

## `open_close.py` Demonstration

`open_close.py` demonstrates how to open and close files in Python.

**Note: `open_close.py` does not contain any tasks to complete**

The crux of the demonstration are the two lines

```python
fileObj = open("open_close.py")
```

and

```python
fileObj.close()
```

This program also demonstrates how to display the current working directory as
well as how to access Python's internal documentation with the `help` function.

Run `python open_close.py` from within the directory that contains it. You can
also try `python -i open_close.py` to open the file in Python's interactive
mode, which will load the contents of the given file and then put you into
Python's REPL.


## Exercise 0

**Read section 0 of [`Reading_Files.md`](Reading_Files.md) before starting this exercise.**

This exercise tasks you with demonstrating your ability to open, read, and close a
file. The provided code should list the current working directory to the user,
as well as prompt them for input to the file. After getting the users input, all
you need to do is:

*   Open the file object of the file `fileName` given by the user.
*   Give the file object to the provided function `getFileAsString`.
*   Print the result of `getFileAsString`, *without printing any additional output and newlines.*
*   Close the file object when finished.

When you have completed `ex0.py`, try running it and opening `./data/text0.txt`,
`./data/text1`, and `./data/text2.mp3`. The contents of these files may expand
your knowledge of the properties of a computer file! In addition to that, try
running `ex0.py` with this directory as your current working directory, as well
as from other directories in the project. This process should assist you in
understanding relative file paths based on the current working directory.

### Test Conditions

The following tests are to be ran against `ex0.py` with `runTests.py`.

```
test_validity:
    Tests whether or not the outputted file of exercise 0 is correct when given a valid path.
```

**PLEASE NOTE THESE TESTS ARE VERY SENSITIVE TO BEING RUN FROM THE DIRECTORY
`2-FileOperations`. These tests will fail if the `runTests.py` script is run
with a current working directory that is not the `2-FileOperations`
directory.**

## Exercise 1

**Read section 1 of [`Reading_Files.md`](Reading_Files.md) before starting this exercise**

If you have not had the pleasurable experience of requesting a non-existing
text file while experimenting with `ex0.py`, you have missed out on
crashing the program with a `FileNotFoundError`.

While there are techniques to catch errors in code *after* they happen, the
best course is to verifying that what we want to do is a good idea *before*
doing it. A smart cliff diver would never take the plunge without first
verifying that the water is deep enough. Be like the smart cliff diver and
look before you leap (even though our stakes are generally much lower).

*   `os.access()` is used to verify the existence of a file
*   `sys.exit()` allows you to quit the program at a specified point


Exercise 1 tasks you with completing the function `getFileSafely`. This function
should do the following:

*   Verify the existence of the file at `path`
*   If the file located at `path` exists and can be `access`ed, `open` the
    file, and return the opened file object.
*   If the file located at `path` does not exist--or cannot be `access`ed--let
    the user know that the file they provided could not be accessed, and that
    they may or may not end up with broken legs next time they look before they
    leap. In other words, exit with "Exit Code 1" using `sys.exit(1)`.

When done correctly the user of this program will never encounter a `FileNotFoundError`.

### Test Conditions

The following tests are to be ran against `ex1.py` with `runTests.py`.

```
test_validity:
    Tests whether or not the outputted file of exercise 1 is correct when
    given a valid path.
test_invalid:
    Tests the output when an invalid file is requested.
```

**PLEASE NOTE THESE TESTS ARE VERY SENSITIVE TO BEING RUN FROM THE DIRECTORY
`2-FileOperations`. These tests will fail if the `runTests.py` script is run
with a current working directory that is not the `2-FileOperations`
directory.**

## Exercise 2

**Read section 2 of [`Reading_Files.md`](Reading_Files.md) before starting this exercise**

After learning the functions `f.read()` and `f.readlines()`, return to complete
the following tasks:

*   Opening a user specified text file like in exercise 0.
*   Read and print the contents of the file using the method of your choice.
    *   This operation will be encased in the function `printContents1`
*   Read and print the contents of the file *again* using a different method.
    *   This operation will be encased in the function `printContents2`
*   Close the file

Given that you are now able to open a file safely, please implement the "look
before you leap" features used in Exercise 1. Your program should not crash --
but should instead exit -- when it encounters an invalid file. You *may* import
the function you created in Exercise 1 for this exercise, or just copy it over.

**IMPORTANT NOTE:** Do *not* print any additional newlines between
`printContents1` and `printContents2`. It *will* cause the tests to fail. More
on this is listed below in the subsection titled "Notes on this exercises unit
tests."

### Test Conditions

The following tests are to be ran against `ex2.py` with `runTests.py`.

```
test_invalid:
    Tests the output when an invalid file is requested.
test_validity_printTwice:
    Tests whether or not the outputted text of exercise 2 `printTwice`
    function is correct when given a valid path.
test_validity_printContents1:
    Tests whether or not the outputted text of exercise 2's `printContents1`
    function is correct when given a valid path.
test_validity_printContents2:
    Tests whether or not the outputted text of exercise 2's `printContents2`
    function is correct when given a valid path.
```

**PLEASE NOTE THESE TESTS ARE VERY SENSITIVE TO BEING RUN FROM THE DIRECTORY
`2-FileOperations`. These tests will fail if the `runTests.py` script is run
with a current working directory that is not the `2-FileOperations`
directory.**


#### Notes on this exercises unit tests

The unit tests for this exercise will test *all* of the standard output. If the
function you utilize from exercise 1 includes extraneous output (ie `print("You
found the file!")`), the test may fail. Remove this additional output, or create
a new `getFileSafely` function for exercise 2 to get around this.

The unit tests for this exercise are sensitive to additional whitespace. Your
printContents functions should print the contents of the specified file and
*nothing* more. By default, Python's `print` function will print the given
string *and* an extra newline character. Whip out the handy-dandy Python REPL,
and take a look at `help(print)` to figure out how to adjust the `end` of a
print statement.

# Reflection and Application

In the above exercises you demonstrated knowledge of opening and closing files,
"looking before you leap" when it comes to opening files, as well as reading
and processing the contents of the file in multiple ways.  Having completed all
of the exercises, you have all of the tools you need to take on the DuckieDecrypter.

Continue on to your implementation of the DuckieDecrypter and apply your
newfound knowledge to develop your first piece of software here at DuckieCorp.
Always feel free to reach out to management if you have any questions.  Be sure
to read through the documentation again if necessary, as well as the Software
Development Plan and chunks of code a previous intern left you.
