# Software Development Plan

# Phase 0: Requirements Specification *(10%)*

* Overview
  * This program dives into the business of secret communication. When a person that doesn't know the algorithm to decrypt a message that has been encrypted they will just see nonsense. This gives us the ability to communicat sensitive data even at the rick of that data being found. As long as the sender and reserver have the DuckieEncryption and DuckieDecryption programs they can understand the data being shared and everyone else that sees the data being confused.
* What this program aims to solve.
  * The program I will be writing is the Decryption of the data. Data that has been encrypted will be passed into the program and will output the decrypted message.
* What a good solution looks like.
  * There should be a low number of keystrokes to get the data inputted to be decrypted. 
  * When the user is on the terminal they can run this program and then as a second argument they can put a file path and if a valid path is given the nothing is printed but the translated text.
  * If the user runs the program in the terminal and there is no second argument then a message indicating to put in a file path for decryption once a valid file path is given then the program prints out nothing but the translated text.
  * There should be clear to understand error messages printed out when invalid file in inputted. (Invalid file meaning permission to read is turned off or the filepath can't be found)
  * When a valid file is inputted then the program should decrypt any valid DuckieCharacters and skip any non-DuckieCharacters.
    * DuckieCharacters
      * begin with a flag ( "_" meaning lowercase character), ( "^" meaning uppercase character), ( "+" meaning a special character)
      * The flag followed by a character code ( "_0" would mean "a" and so-forth until "_25" would mean "z"), ( "^0" would be "A" and so-forth until "^25" would mean "Z"), ( "+A0" would mean " " and so-forth until "+A32" would mean "@". There are also B and C Groups for special characters for example "+B0" would be "[")
      * All DuckieCharacters are separated by a space.
      * There is a list provided in the documentation of this phase that was writen by Erik Falor that will list all the DuckieCharacters and there meaning in english. 
* Things I already know how to do.
  * I know how to see if the file gives me permission to read it, and I know how to exit the program with an error message stating what went wrong.
  * I know how split up strings by spaces and put the individual words into a list.
  * I know how to access the elements in a string and makes tests on those elements.
* Challenges I foresee.
  * It's been a while since I have written a full fledge program in python so I might have issues thinking of all the syntax.
  

## Documentation For This Phase

### Uppercase Characters

| Character      | DuckieCrypt Translation
| :------------- | :---------------------
| `A`            | `^0`
| `B`            | `^1`
| `C`            | `^2`
| `D`            | `^3`
| `E`            | `^4`
| `F`            | `^5`
| `G`            | `^6`
| `H`            | `^7`
| `I`            | `^8`
| `J`            | `^9`
| `K`            | `^10`
| `L`            | `^11`
| `M`            | `^12`
| `N`            | `^13`
| `O`            | `^14`
| `P`            | `^15`
| `Q`            | `^16`
| `R`            | `^17`
| `S`            | `^18`
| `T`            | `^19`
| `U`            | `^20`
| `V`            | `^21`
| `W`            | `^22`
| `X`            | `^23`
| `Y`            | `^24`
| `Z`            | `^25`


### Lowercase Characters

| Character      | DuckieCrypt Translation
| :------------- | :---------------------
| `a`            | `_0`
| `b`            | `_1`
| `c`            | `_2`
| `d`            | `_3`
| `e`            | `_4`
| `f`            | `_5`
| `g`            | `_6`
| `h`            | `_7`
| `i`            | `_8`
| `j`            | `_9`
| `k`            | `_10`
| `l`            | `_11`
| `m`            | `_12`
| `n`            | `_13`
| `o`            | `_14`
| `p`            | `_15`
| `q`            | `_16`
| `r`            | `_17`
| `s`            | `_18`
| `t`            | `_19`
| `u`            | `_20`
| `v`            | `_21`
| `w`            | `_22`
| `x`            | `_23`
| `y`            | `_24`
| `z`            | `_25`


### Special Characters

#### Group A
| Character      | DuckieCrypt Translation
| :------------- | :---------------------
| ` `            | `+A0`
| `!`            | `+A1`
| `"`            | `+A2`
| `#`            | `+A3`
| `$`            | `+A4`
| `%`            | `+A5`
| `&`            | `+A6`
| `'`            | `+A7`
| `(`            | `+A8`
| `)`            | `+A9`
| `*`            | `+A10`
| `+`            | `+A11`
| `,`            | `+A12`
| `-`            | `+A13`
| `.`            | `+A14`
| `/`            | `+A15`
| `0`            | `+A16`
| `1`            | `+A17`
| `2`            | `+A18`
| `3`            | `+A19`
| `4`            | `+A20`
| `5`            | `+A21`
| `6`            | `+A22`
| `7`            | `+A23`
| `8`            | `+A24`
| `9`            | `+A25`
| `:`            | `+A26`
| `;`            | `+A27`
| `<`            | `+A28`
| `=`            | `+A29`
| `>`            | `+A30`
| `?`            | `+A31`
| `@`            | `+A32`

#### Group B

| Character      | DuckieCrypt Translation
| :------------- | :---------------------
| `[`            | `+B0`
| `\`            | `+B1`
| `]`            | `+B2`
| `^`            | `+B3`
| `_`            | `+B4`
| `` ` ``        | `+B5`

#### Group C

| Character      | DuckieCrypt Translation
| :------------- | :---------------------
| `{`            | `+C0`
| `\|`           | `+C1`
| `}`            | `+C2`
| `~`            | `+C3`


# Phase 1: System Analysis *(10%)*


* What data does my program use and where does it come from?
  * This program takes in files that have text. If while reading the text file the programs sees any DuckieCharacters it will translate it into english and skip over the non-DuckieCharacters.
  * As long as the program gets a valid file path and the file gives reading permission then the data could come from anywhere on the computer the program is installed.
* What should the output look like?
  * The output will look much like the duckieEncrypter program writen by Erik Falor so that both programs look good when used together to encrypt and then decrypt, I will list what that looks like in the documentation part of this phase.
* What algorithms and formulae will be used in this program?
  * The program will be split up into its individual parts and each part will be delt with separately.
    * The program will have to be able to tell what is a good file path and what is not.
    * The program will have to check, once the file path is confirmed to be valid, if the file gives reading permissions.
    * The program will be using the ASCII table with converting from characters to numbers and vis-versa. 
    * The program will need to be able to read in the contents of the file and split up words by a space so that each word or character can be looked at and all items put into a list.
    * The program will need to be able to tell what is a valid DuckieCharacter and what is not so that it can skip over the unneeded text.
    

## Documentation For This Phase
$ indicates the users command line in the terminal.

There are two ways to use the program, the user can run the program and then as a second argument put the file path they want decrypted. If they put a second argument after running the program then the program will output the decrypted message and then end. If they dont put a second argument then they are prompted to put in a file path. 

### For when the user puts in a valid argument when the program is run

User:
```
$ python src/duckieDecrypter.py example/example.txt
```

Program:
```
The text printed here will be the decrpted txt file excluding non-DuckieCharacters
```


### For when the user doesn't put in a argument when the program is run

User:
```
$ python src/duckieDecrypter.py
```

Program: 
```
Your current working directory is:
  /Users/nathanstott/CS1440/cs1440-assn1/src
File to decrypt: 
```

User:
```
>>> example/example.txt
```

Program:
```
The text printed here will be the decrpted txt file excluding non-DuckieCharacters
```

### Invalid file path or can't read file

If at any point when prompted for a file path the user puts in a file that doesn't have read permissions enabled or gives a invalid path this message is displayed.

Program:
```
Error! The supplied text file does not exist or is not accessible.
```

# Phase 2: Design *(30%)*


* Function Signatures
  * Descriptive name: main
    * Parameter list: filePath
    * Purpose: The part of the program that starts orchestrating everything
    * inputs: What the user gives as a file path
    * outputs: prints each line of the file 
  * Descriptive name: getFile
    * Parameter list: pathToFile
    * Purpose: Checks to see if the file path given by the user is valid 
    * inputs: the path to the file the user gave
    * outputs: either an error message and quits or returns a file object
  * Descriptive name: decryptLine
    * Parameter list: line
    * Purpose: Decrypt a line 
    * inputs: A line of the file 
    * outputs: A line of decrypted duckieCrypt text
  * Descriptive name: decryptCharacter
    * Parameter list: character
    * Purpose: Decrypt a single character 
    * inputs: The character to be decrypted
    * outputs: A decrypted duckieCrypt Character or an empty string
  * Descriptive name: convertToSpecialChar
    * Parameter list: charCode
    * Purpose: Convert a given character code to the corresponding decrypted special character
    * inputs: A character code
    * outputs: special character
  * Descriptive name: convertToUpper
    * Parameter list: charCode
    * Purpose: Convert a given character code to the corresponding decrypted uppercase character
    * inputs: A character code
    * outputs: Uppercase letter
  * Descriptive name: convertToLower
    * Parameter list: charCode
    * Purpose: Convert a given character code to the corresponding decrypted lowercase character
    * inputs: A character code
    * outputs: Lowercase letter
  * Descriptive name: sendError
    * Parameter list: msg=None, exitCode=1
    * Purpose: To print to the screen an error message saying that the file wasn't valid
    * inputs: message to print, what code to exit with
    * outputs: prints given message and then exits


* Pseudocode for main:
  * create a file object by calling getFile.
  * put all the lines in the file object in a list and take each line at a time for decryption and then print the result by calling decryptLine.
  * Once all the lines have been decrypted close the file object and stop the program.
  * What happens with good input and bad input:
    * Good
      * the program should print out the decrypted message.
    * Bad
      * the program should print out a message saying that the file given is an invalid filepath and then exit.
    * Examples:
      * Good
        * Python3 duckieDecrypter.py data/exampleFilePath.txt
      * Bad
        * Python3 duckieDecrypter.py data/imNotReal.txt
* Pseudocode for getFile:
  * we need to check if we have access to the path that is given and if it is then we can return the file object.
  * if it's not a valid path or we don't have access then we need to call sendError.
  * What happens with good input and bad input:
    * Good
      * The program should see that the file gives read permission and return a file object.
    * Bad
      * The program should see that the file doesnt exist or that the file doesnt give read permission and then call sendError.
    * Examples:
      * Good
        * data/exampleFilePath.txt
      * Bad
        * data/iDontGivePermissionToRead.txt
* Pseudocode for decryptLine:
  * go through all the words in the line given.
  * returns the whole line after decryption
  * What happens with good input and bad input:
    * Good
      * the line gets decrypted
    * Bad
      * doesn't add anything to the return and nothing gets printed out
    * Examples:
      * Good
        * line contains text
      * Bad
        * line contains nothing
* Pseudocode for decryptCharacter:
  * if the start of a word starts with ^ then call convertToUpper and pass in the word excluding the first character.
  * else if the start of a word starts with _ then call convertToLower and pass in the word excluding the first character.
  * else if the start of a word starts with + then call convertToLower and pass in the word excluding the first character.
  * if the convertedChar is not equal to '' then add the convertedChar to the output with a space.
  * What happens with good input and bad input:
    * Good
      * calls the correct method so that the character code can be converted to a letter or special character
    * Bad
      * doesn't add anything to the return and nothing gets printed out
    * Examples:
      * Good
        * the character passed in should be a number 
      * Bad
        * the character passed would not be a number
* Pseudocode for convertToSpecialChar:
  * if the charCode's first char is A and the rest of the charCode is >= 0 and <= 32 then return the ord of the rest of the charCode + 32
  * if the charCode's first char is B and the rest of the charCode is >= 0 and <= 5 then return the ord of the rest of the charCode + 91
  * if the charCode's first char is C and the rest of the charCode is >= 0 and <= 3 then return the ord of the rest of the charCode + 123
  * What happens with good input and bad input:
    * Good
      * returns the converted char
    * Bad
      * returns nothing
    * Examples:
      * Good
        * A4
      * Bad
        * A100
* Pseudocode for convertToUpper:
  * if the charCode is >= 0 and <= 25 then return the ord of the charCode + 65 
  * else return nothing
  * What happens with good input and bad input:
    * Good
      * the returned char will be a upper case letter
    * Bad
      * return nothing
    * Examples:
      * Good
        * 20
      * Bad
        * 100
* Pseudocode for convertToLower:
  * if the charCode is >= 0 and <= 25 then return the ord of the charCode + 97
  * else return nothing
  * What happens with good input and bad input:
    * Good
      * the returned char will be a lower case letter
    * Bad
      * return nothing
    * Examples:
      * Good
        * 10
      * Bad
        * 100
* Pseudocode for sendError:
  * if there are no arguments given then the default message is printed and the exit code is 1
  * What happens with good input and bad input:
    * Good
      * the default message is printed out
    * Bad
      * a nonsense message is printed out
    * Examples:
      * Good
        * nothing is passed in 
      * Bad
        * a number is passed in for the message
        


# Phase 3: Implementation *(15%)*


* (More or less) working Python code in `src/`.
  * This has been completed
* Note any relevant and interesting events that happened while you wrote the code.
  * While writing the code everything went well, but while I was writing I did need to check in the char conversion methods that the charCode is a number. It was refreshing to write some python code and get back in the groove.
  
  
# Phase 4: Testing & Debugging *(30%)*


* A set of test cases that you have personally run on your computer.
  * I ran everything in the data file including the test file.
  * Include a description of what happened for each test case.
    * For every file I put in, the program ran smoothly with no errors.
  * For any bugs discovered, describe their cause and remedy.
    * When I was testing if the charCode was greater than 0 and less than 25 or whatever it would break. I found it was breaking because the charCode was being passed in as a string and it needed to be converted to an int to do that kind of operation.
    * I also found that I was doing ord(charCode) when I needed to use chr(charCode) to get the right outcome.
    * I also found that I needed to check for the converting to special characters that I needed to check if there was nothing on the other side of the group letter.
    * needed to change my or's to and's in my if statements when checking for if the number was in a range.
* Write your test cases in plain language such that a non-coder could run them and replicate your experience.
  * I ran everything in the data file and everything printed seemingly normal. Below are some examples of the input and output I got
  
While running these commands I am in the terminal at this location "/Users/nathanstott/CS1440/cs1440-assn1/src".

Input:
```
$ python3 duckieDecrypter.py /Users/nathanstott/CS1440/cs1440-assn1/data/secret0.txt
```
Output:
```
I'd never do it to you. I'd never let you down like that.
https://youtu.be/dQw4w9WgXcQ
```

---------

Input: 
```
$ python3 duckieDecrypter.py /Users/nathanstott/CS1440/cs1440-assn1/data/msg0.txt   
```
Output: 
```
Welcome to DuckieCorp! We sure are glad to have you working with us. In your
tenure here, we hope you will learn many new techniques to enhance your computer
science and problem solving skills. We're excited to get started!
- DuckieCorp Management
```

-----------

Input: 
```
$ python3 duckieDecrypter.py /Users/nathanstott/CS1440/cs1440-assn1/data/test/allChars.txt
```
Output: 
```
 
!
"
#
$
%
&
'
(
)
*
+
,
-
.
/
0
1
2
3
4
5
6
7
8
9
:
;
<
=
>
?
@
A
B
C
D
E
F
G
H
I
J
K
L
M
N
O
P
Q
R
S
T
U
V
W
X
Y
Z
[
\
]
^
_
`
a
b
c
d
e
f
g
h
i
j
k
l
m
n
o
p
q
r
s
t
u
v
w
x
y
z
{
|
}
~
```


# Phase 5: Deployment *(5%)*


* Your repository pushed to GitLab. 
  * DID IT
* **Verify** that your final commit was received by browsing to its project page on GitLab.
  * DID IT
  * Review the project to ensure that all required files are present and in correct locations.
    * DID IT
* **Validate** that your submission is complete and correct by cloning it to a new location on your computer and re-running it.
  * DID IT
  * Run through your test cases to avoid nasty surprises.
    * DID IT

# Phase 6: Maintenance


*   Write brief and honest answers to these questions: *(Note: do this before you complete **Phase 5: Deployment**)*
    * What parts of your program are sloppily written and hard to understand?
      * I dont think that any parts of my program are written sloppily or hard to understand but maybe im biased because I wrote it.
        * Are there parts of your program which you aren't quite sure how/why they work?
          * There are not I understand everything in the program.
        * If a bug is reported in a few months, how long would it take you to find the cause?
          * Very fast because the program isn't big.
    * Will your documentation make sense to
        * anybody besides yourself?
          * I think so anyone that plays around with the program for a second would understand.
        * yourself in six month's time?
          * I would it's not that complicated.
    * How easy will it be to add a new feature to this program in a year?
      * Very easy I will just need to read through the code and then implement the new feature.
    * Will your program continue to work after upgrading
        * your computer's hardware?
          * Yes
        * the operating system?
          * Yes
        * to the next version of Python?
          * Yes
        
