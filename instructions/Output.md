# CS 1440 Assignment 1: DuckieDecrypter - Sample Output

This document shows what output your DuckieDecrypter program should produce under various circumstances.

*   


## When No Parameters Are Supplied

There should be an interactive prompt that prompts the user for the file to
decrypt. This file should then be decrypted and have it's output printed. A 
message may be displayed showing the current working directory.

```
$ python src/duckieDecrypter.py 
Your current working directory is:
  /home/jaxtonw/Documents/USU/1440/cs1440-falor-erik-assn1
File to decrypt: data/msg0.txt
Welcome to DuckieCorp! We sure are glad to have you working with us. In your
tenure here, we hope you will learn many new techniques to enhance your computer
science and problem solving skills. We're excited to get started!
- DuckieCorp Management
```

**NOTE: This prompt is given in the starter code in the `if __name__ ==
"__main__":` block.** The value input by the user is stored in the variable
`filePath`, which is then given to the `main` function.


## When A File Is Given As A Command Line Argument

There should be *no* interactive prompt or message about the current working
directory. The file should then be decrypted and have it's output printed as the
*only* output of the program.

```
$ python src/duckieDecrypter.py data/msg0.txt 
Welcome to DuckieCorp! We sure are glad to have you working with us. In your
tenure here, we hope you will learn many new techniques to enhance your computer
science and problem solving skills. We're excited to get started!
- DuckieCorp Management
```

**NOTE: This is given in the starter code in the `if __name__ == "__main__":`
block.** The value input by the user is stored in the variable `filePath`, which
is then given to the `main` function. If multiple files are given on the command
line, only the first file is processed.


## When A File That Does Not Exist Is Requested

If a file does not exist (or cannot be properly accessed with read permissions),
the program should quit with an error message detailing that the file could not
be read. If the program was run with no arguments, the interactive prompt output
will be shown as well. If the file path was given as a command line argument,
there should only be the error message printed. 

```
$ python src/duckieDecrypter.py data/FILEDNE
!!!QUACK!!!
================================================================================
File located at data/FILEDNE cannot be read.
================================================================================
!!!QUACK!!!
```


## When An Existing File Containing *Only* Invalid DuckieCrypt Is Requested

The program will not produce any output, aside from newlines that are to be
preserved.

```
$ python src/duckieDecrypter.py data/test/invalid0.txt 




```

## When An Existing File Containing Valid *And* Invalid DuckieCrypt Is Requested

The program will produce output for the valid DuckieCrypt, and skip over the
invalid DuckieCrypt. Newlines will be preserved.

```
$ python src/duckieDecrypter.py data/test/invalid2.txt 
The invalid characters shouldn't mess with me!
```


## When The User Gives The Command Line Argument `-h` or `--help`

A usage message should be printed.

```
$ python src/duckieDecrypter.py --help
USAGE:
  $ python src/duckieDecrypter.py [FILE PATH]

DESCRIPTION:
  The DuckieDecrypter is a proprietary tool created by DuckieCorp to *decrypt*
    messages that are composed of DuckieCrypt. That is, to turn DuckieCrypt
    back into plain text.

ARGUMENTS:
  [FILE PATH] : Optional
    Specify the path to a file containing DuckieCrypt to decrypt.
    If this argument is not given, then the user is prompted for manual input
      for the file path.
    Only one file can be specified.

  -h | --help
    Produce this help message if given as any argument to the program. 
```

**NOTE: This *entire* functionality is given in the starter code.** You do not
need to worry about this behavior.
