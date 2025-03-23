# Text Processing

This lesson focuses on processing text as a whole, rather than exclusively the character contents of a string. Throughout this lesson you will be dealing with the content of strings, sorting this data, eliminating unnecessary data, and decomposing this data into more manageable pieces.


## Exercise 0 -- [`ex0.py`](./ex0.py)

Create a function `findWords` that iterates through a given string, returning a list of all *space separated* sub strings with 5 characters or less. In other words, when given a sentence, return a list of all *words* that are 5 characters or less. All characters in a string count towards the character count, including punctuation, and excluding spaces.

```python
Input 0: "A sentence has been given to the function!"
Output 0: ["A", "has", "been", "given", "to", "the"]

Input 1: "Keep Keep, Don't!"
Output 1: ["Keep", "Keep,"]

Input 2: "Nothing useful!"
Output 2: []
```

### `str.split()` Method

On a Python string object, there exists a `split()` method which splits a given string based on a given substring. Using the Python REPL, execute `help(str.split)` to view more information on the `split` function. This function will be very handy on this exercise!

### Test Conditions

The following tests are to be ran against `ex0.py` with `runTests.py`.

```
test_validity:
    Tests the validity of the output.
        EX: findWords(["keep reject"]) == ["keep"]
test_emptyList:
    Tests the validity of the output when an empty list is expected.
        EX: findWords("reject") == []
            findWords("") == []
```


## Exercise 1 -- [`ex1.py`](./ex1.py)

Create a function that returns a string containing every other word in the
given sentence. The sentence will be given as a string, containing letters and
punctuation separated by space characters ` `.  Construct and return a string
that reports `everyOtherWord` in the sentence.

*   Your implementation of the `everyOtherWord` function includes every other
    word in the resulting output, *excluding* the first word. The very first
    word in the resulting sentence should be the 2nd word of the input sentence.
*   Punctuation should be preserved.
*   There should be a space character between each word.
*   A trailing space character is allowed at the end of the output string.

```
Input 0: "A string of words."
Output 0: "string words."

Input 1: "A bit of redundancy is nice when it comes to understanding."
Output 1: "bit redundancy nice it to"

Input 2: "OneWord"
Output 2: ""
```

### Test Conditions

The following tests are to be ran against `ex1.py` with `runTests.py`.

```
test_validity:
    Tests whether or not the output of exercise 1 is correct. Extra white
    space at the start or end of a string will not affect the tests.
        EX: everyOtherWord("1 2 3 4 5 6 7 8") == "2 4 6 8"
test_emptyString:
    Tests if a given sentence that should return an empty string does
    return an empty string.
        EX: everyOtherWord("OneWord") == ""
            everyOtherWord("") == ""\
```


## Exercise 2 -- [`ex2.py`](./ex2.py)

Clean excess words from of a supplied string.  Your function `cleanSentence`
will return a list containing the words remaining in the provided string, with
words meeting select criteria removed.  All words starting with `#` should be
excluded from the output.

*   A word is any collection of characters separated by spaces, as in Exercise 1.
*   Punctuation should be preserved in the output.
*   There should be a space character between each word in the sentence.
*   The order of the words in the output should be preserved from the input.

```
Input: This #Contains sentence #some #other is #input #too. output.
Output: ["This", "sentence", "is", "output."]
```

Ensure that if an empty string is given, an empty list is returned.

```
Input:
Output: []
```

### Python Strings Are Basically Just Lists!
In Python, string objects share many features with lists. The access and slice
operations that exist on lists exist on strings in Python. This makes working
with Python strings relatively easy! One can use the bracket characters `[]` and
an integer to access a specific character in a string, and the `:` operator in
the brackets to obtain *slices* of said list. Strings accessing also suffers
similar fates to lists when it comes to accessing an item that is not within the
bounds of the list, so be wary!

One can open the REPL and follow along with the following Python experiments themselves.

#### Access the first character of a string
```py
>>> "hello"[0]
"h"
```

#### Access the last character of a string
```py
>>> "hello"[-1]
"o"
```

#### Obtaining a copy of a string from the 3rd charater on
```py
>>> "hello"[2:]
"llo"
```

#### Obtaining a copy of a string from the beginning *until* the last character
```py
>>> "Just think about why I have to change this string here..."[:-1]
"Just think about why I have to change this string here.."
```

#### Obtaining a copy of a string from the 3rd character *until* the last character
```py
>>> "hello"[2:-1]
"ll"
```

#### Index out of bounds on a string
```py
>>> "hello"[10]
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
IndexError: string index out of range
```


### Test Conditions

The following tests are to be ran against `ex2.py` with `runTests.py`.

```
test_emptyList:
    Tests that the function returns an empty list when expected.
        EX: cleanSentence("#Nothing") == []
test_validity:
    Tests the validity of the output with a list as input.
        EX: Ensures cleanSentence("What #do you expect?") == ["What", "you", "expect?"]
```

## Exercise 3 -- [`ex3.py`](./ex3.py)

This exercise is similar to exercise 2.

*   Create a function which parses through a sentence, returning lists of select content.
*   However, instead of eliminating the words that were removed, return them in a second list.
*   Separate words of the sentence into 2 groups, `clean` and `dirty`.
    *   `dirty` is a list of words marked with the special character `#`.
    *   `clean` contains words that are not marked with `#`.
    *   Return a list of lists, `[clean, dirty]`.
        *   Please note that in the provided code is the statement `return (clean, dirty)`. As `clean` and `dirty` were earlier declared to be lists, this will return a *tuple* containing the two lists, `clean` and `dirty`. This means that this requirement has already been met!
*   When adding a word to the `dirty` list, be sure to add it *with the special
    character removed from the word.*
*   When an empty string is given *both* lists are empty.

```
Input: This #Contains sentence #some #other is #input #too. output.
Output: (["This", ...], ["Contains", ...])
Output[0]: ["This", "sentence", "is", "output."]
Output[1]: ["Contains", "some", "other", "input", "too."]
```

### Test Conditions

The following tests are to be ran against `ex3.py` with `runTests.py`.

```
test_validity_both:
    Verifies that both lists returned contain the correct output for the given input.
        EX: cleanSentenceTwoLists("Clean #Dirty") == (["Clean"], ["Dirty"])
test_validity_clean:
    Verifies that the clean list returned contains the correct output for the given input.
        EX: cleanSentenceTwoLists("Clean #Dirty")[0] == ["Clean"]
test_validity_dirty:
    Verifies that the dirty list returned contains the correct output for the given input.
        EX: cleanSentenceTwoLists("Clean #Dirty")[1] == ["Dirty"]
test_removalOfCharFromDirty:
    Verifies that the special character is removed from the contents of the `dirty` list.
        EX: cleanSentenceTwoLists("#Dirty")[1] == ["Dirty"]
test_emptyList_both:
    Verifies that an empty list is returned for both the `clean` and `dirty` list when expected.
        EX: cleanSentenceTwoLists("") == ([], [])
test_emptyList_clean:
    Verifies that an empty list is returned for the `clean` list when expected.
        EX: cleanSentenceTwoLists("#Only #Dirty")[0] == []
test_emptyList_dirty:
    Verifies that an empty list is returned for the `dirty` list when expected.
        EX: cleanSentenceTwoLists("Only clean")[1] == []
```

# Reflection and Application

In these exercises, you learned how to loop through a list of words, and keep
only those that meet select criteria. You also learned how to split a line of
text into individual words using the `str.split()` function. In addition to
that, you worked on looping through multiple sentences, only keeping select
words in those sentences. You've come a long way in your work on text
processing, and many of these skills you have come to develop are directly
applicable to your DuckieDecrypter.

How are these applicable? Well, DuckieCrypt is a series of characters in a text
file separated by spaces. We know how to handle that. We also now know how to
look at a word, and determine whether or not it meets select criteria: which we
can use to verify that DuckieCrypt characters are valid while decrypting. We
also have learned how to look at the first character of a word, and determine
whether or not we want to keep the word in our sentence or not. How different is
this to the flags we must observe in a DuckieCrypt character?

Completing these exercises has hopefully helped prepare you for the
DuckieDecrypter you are tasked with creating. Continue on to the last practice
exercises, located at [../2-FileOperations](../2-FileOperations), that will
teach you how to properly handle files, and read the contents of them.
