# Section 0

Please read the following section before completing exercise 0.

## Reading files in Python

Many programs you'll write this semester will take as command-line
arguments names of files that your program must process.

These are a few basic operations that all programming languages allow you to
perform on files:

* Open
* Read
* Write (we'll do this later)
* Close


### `f = open()`

The `open()` function takes as an argument a string naming a file and returns
a file object.  The file object (which I called `f`) represents the file and,
through it, we may perform the other three file operations.

If there is any problem accessing or opening the file named by the string
argument, `open()` will raise an error which may terminate your program.  There
are two approaches you can take to these errors:

0.  *Look Before You Leap* Avoid trouble by detecting possible problems before
    operating on the file.
1.  *It's Easier To Ask Forgiveness Than To Get Permission* Jump in with both
    feet and worry about the consequences later.  It may be acceptable to let
    the program crash in the face of some problems.  Or, your program may
    attempt to recover from errors as they occur.  Python and other modern
    programming languages achieved this through a mechanism known as *Exception
    Handling*.

We'll use the *look before you leap* approach in Assignment #0 by using the
`os.access()` function. This function will be demonstrated in a later exercise.  We'll explore exception handling in more depth in later assignments.

### `f.close()`

Software is limited by the OS in the number of files it can hold open at a
time.  Once reached, subsequent calls to `open()` will fail until the number of
open files is reduced.

After you are finished using a file it is good programming hygiene to close the
file with the file object's `.close()` method.  This is something that I and
the graders will be on the lookout for.

*   What is the consequence of not closing files that you are finished with?
*   Is it okay to leave a file open if you are still using it?

## The Current Working Directory (CWD) and your code

### Files and Paths

Files on a computer are named by their *path*.

The *path* to a file or directory may include information about which
directories contain the object.

There are two kinds of paths: [Absolute and Relative](./Absolute_vs_Relative_Paths.md)

### Rule of thumb: don't make assumptions about files and directories

Write programs such that the user is fully responsible for specifying the path
to input files.  Your program should *not* encode any information about files
or directories on your computer.  It is too much information (TMI) and makes
your program less portable.

Do *not* write code that does this:

    filename = input("Gimme the name of a file: ")
    if os.access("../" + filename, os.R_OK):
        f = open("../" + filename)
        for line in f:
            pass

This snippet of code only works when data files are stored at a path that is up
one level from the CWD.  If you run this program from a different location on
the computer it will unexpectedly fail to find its files.

### The CWD is inherited by child processes

The prompt of the command shell indicates your current working directory (CWD).
The CWD affects the behavior of programs by influencing how they may refer to
the files they access.

*   What effect do you think the CWD has on the DuckieDecrypter program?

*   How can you control the CWD of `DuckieDecrypter.py`...
    *   on the command line?
    *   in PyCharm?



# Section 1

Please read the following section before completing exercise 1. This section is
devoted to guaranteeing the existence of a file *before* attempting to open said
file.

## `os.access()`

The `os` module provides the handy function, `access`. This function returns a
boolean that can be used to determine if the user has permissions to access a
specified file with select permissions.

`os.access` takes two required parameters, `path` and `mode`. `path` is the path
to the specified file, while `mode` is the permissions in question. For
DuckieCrypt, we are only concerned about Read Access, so using `os.R_OK` for the
mode will work for now. To check for read permissions of a file located at
`[FILEPATH]`, one would use `os.access('[FILEPATH]', os.R_OK)`

## `sys.exit()`

The `sys` module provides a convenient method for exiting the program. Typically
used to exit the program under non-standard circumstances, `sys.exit()` can be
used to exit a program in the event that *something* isn't going right, like an
erroneous file path *hint hint*. `sys.exit()` takes one optional parameter,
`status`. If left blank or as 0, the program exits with "Exit Code 0", which
conventionally means the program exited with no issues. Adding a non-zero
`status` to the exit function, like `sys.exit(1)` will return an "Exit Code" of
`status`. "Exit Code 1" is convention for *something* went wrong during the
execution of a program.


# Section 2

Please read the following section before completing exercise 2. This section is
devoted to reading the contents of a file object.

## `f.read()`

You can read any quantity of data from a file up to the number of bytes
contained therein.  To read a particular number of bytes use the `.read()`
method, passing that number as the argument:

    tenBytes = f.read(10)
    twentyBytes = f.read(20)

It is not an error to try to read beyond the end of the file.  For example, if
you call `f.read(100)` and there are only 30 bytes available, you get those
remaining 30 bytes.

Python keeps a cursor within the file object to remember where you were at the
last time you read from the file.  Each time you call `.read()` this cursor is
advanced automatically until you reach the end of the file.

`.read()` returns the chunk of data as a String value.

When the end-of-file (EOF) is reached `.read()` returns the empty string,
which acts as a sentinel value.  This is the only time that `.read()` can
return an empty string, and is the way your program will detect that it has
reached the end of the file.

#### [Sentinel Value](https://en.wikipedia.org/wiki/Sentinel_value)

A special value in the context of an algorithm whose presence is a condition of termination


If you want to *slurp* the entire file into a variable in one go, give
`.read()` an empty argument list *or* a negative number.

*   Using the REPL, can you find a method that allows you to rewind the file to
    the beginning so as to re-read it?

## `f.readline()`

When we know that our file contains lines of text it is more convenient to read
it one line at a time.  `.readline()` will read bytes from the file until it
reaches an end-of-line (EOL) sequence or EOF.  `.readline()` returns a string
which includes the EOL sequence.  As with `.read()` the empty string `''` is
returned when EOF is reached.

#### EOL sequence varies by system

EOL == `"\n"` on Unix and MacOS

EOL == `"\r\n"` on Windows


*   How will your program know when it has reached the EOF?  Try this out in
    the REPL.
*   What happens when you use `.readline()` on a non-text file? Find yourself
    a `.png` image on your computer. What happens when you read it?
*   Because the string resulting from `.readline()` already contains the EOL
    sequence, printing it with `print()` causes an extra blank line to appear.
    How can you prevent this from happening?

## `f.readlines()`

The sister method to `f.readline()`. Instead of returning only one line from the
file each time the function is called, `f.readlines()` will return an array of
*all* the lines in the file. This operation is convenient, however it requires
the loading of the entire contents of the text file into computer memory. When
working with small text files, the affects are negligible, but on large text
files, Python may crash due to running out of memory while attempting to load
the entire file into an array.


## `f.seek()`

A method used on a file to change the current stream position to a specified
byte. `f.seek(0)` will seek the 0th byte of file `f`, starting the stream back
at the beginning of a file. In the event one wants to read the contents of the
same file object multiple times, the `seek` method will likely be utilized.
