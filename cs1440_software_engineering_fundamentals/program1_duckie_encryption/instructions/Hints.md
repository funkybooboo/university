# CS 1440 Assignment 1: DuckieDecrypter - Hints

*   [Erik's Hints](#eriks-hints)
*   [Students' advice from last semester](#students-advice-from-last-semester)


## Erik's Hints

0.  Spend some time studying [`demo/duckieEncrypter.py`](../demo/duckieEncrypter.py) to build your understanding of DuckieCrypt.  When running that program from the command line, add the `--help` option to show a detailed usage message.
1.  ASCII characters are significant to DuckieCrypt.  In fact, the mapping of the integer portions of the character codes follow similar patterns as the mapping of ASCII characters.  Keep your [ASCII character table](https://www.asciitable.com/) handy!
2.  A typical implementation of DuckieDecrypter can be made with very little code (around 100 lines).  Don't over-think this assignment, nor write redundant functions that do the same thing as Python's built in-tools.
3.  Familiarize yourself with these key functions, which are integral to completing the DuckieDecrypter as well the lessons in [`lsn/`](../lsn/).
    * `chr()`
    * `ord()`
    * `int()`
    * `len()`
    * `open()`
    * `close()`
    * `os.R_OK`
    * `os.access()`
    * `print()`
    * `str.split()`
    * `str.isdigit()`
    * and many more!
4.  Run `help([function_name])` in the REPL to learn more about the functions you will use:
    ```
    $ python -i
    >>> help(print)
    Help on built-in function print in module builtins:

    print(...)
    print(value, ..., sep=' ', end='\n', file=sys.stdout, flush=False)
    ...

    >>> help(str.split)

    Help on method_descriptor:
    split(self, /, sep=None, maxsplit=-1)
    Return a list of the words in the string, using sep as the delimiter string.
    ...
    ```
5.  After you develop the logic that identifies DuckieCode flags, how much different will it be to recognize the special characters group?
6.  Be sure to test the DuckieDecrypter against invalid characters!  Run your program with the files [`data/test/invalid0.txt`](../data/test/invalid0.txt), [`data/test/invalid1.txt`](../data/test/invalid1.txt), and [`data/test/invalid2.txt`](../data/test/invalid2.txt).


## Students' advice from last semester

In last semester's feedback 65/124 students' mention "start early", and 26/124 warn against procrastination.  Seems like there's a common thread in the advice from our previous interns...


### Read before you write

*   Do the lessons, read the encrypter program, write your plan, then write the decrypter in that order
*   Write the plan. It may seem like extra work, but it does help make life a lot easier.
*   Plan everything before hand. It's way easier to come up with ideas and think things through without having to worry about syntax.
*   Read and think about what the assignment is asking for.  I misinterpreted the assignment so I ended up writing code that I didn't need. 
*   Read the function templates first in `src/duckieDecrypter.py`
*   There were so many files in regard to instructions scattered that I missed the `UnderstandingDuckieCrypt.md` doc and it caused me to plan for a completely different thing than was explained there, was stressful when I figured out I had to write code very different from my pseudo code
*   Take some notes on the lessons, particularly when it comes to using files. It gets troublesome needing constantly navigate back and forth constantly.  Along with that, use physical paper to help plan. Physical paper is still produced for a reason.


### Time management

*   Work on it little by little over the span of many days. Spend more time on the plan than on the programming.
*   Take the time to do the lessons, it seems like a lot of work but if you spread it out it's not too bad and it makes the assignment a lot easier. A lot of the lesson stuff is pretty much exactly what you need to finish the assignment.
*   I worked ahead so that I could have a lot of time to debug and figure out little aspects of the code that I hadn't considered.
*   For me the biggest setback was a simple logic error I had made, that took me way too long to find.  That sucked up a lot of my time, and if I had just been a little more careful in my original plan I might've been able to avoid it.


### Debugging

*   Adding a lot of extra print statements while bug testing really helped me follow the data through the program, and also allowed me to see where it was being manipulated differently than I intended in my pseudocode.
*   There was an error I kept receiving and I could not for the longest time figure out why--finally I used print statements to test and find where the error was occurring which allowed me to know how to fix it
*   Repeatedly test and debug the code for each possible encrypted file, and as I got each to work, going back and ensuring that previous tests still decrypted properly until all of them were decrypted.


### Getting help

*   Look at the discord, other students have also probably faced the same problems.
*   Make connections with your classmates and reach out to them for help. Start the assignment well in advance and do a little bit each day.
*   This may be obvious but a lot of the assignment can just be lifted from the lessons that you did before starting the decrypter.
*   If you've taken 1400 and have some of your old homework, a brief look through of your old python code might be a good refresher on python syntax.
