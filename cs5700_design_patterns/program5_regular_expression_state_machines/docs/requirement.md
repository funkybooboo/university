Assignment 5: Regular Expressions

Regular expressions are used for describing languages (or a list of valid strings for a language). In this assignment you are going to build state machines that can detect strings of the following string shapes:

    An integer
    A floating point value
    A binary number that starts and ends with a 1
    An email address
    A password that meets complexity requirements.

You are not building a regular expression compiler, rather you will design purpose built state machines for each of those patterns. You cannot use regular expressions at all in this assignment to detect those patterns. So why is this assignment called "Regular Expressions"? Well, when you use a regular expression it compiles to a state machine exactly like the ones you will be designing by hand that do the exact same things.

You do not need to build an executable for this and can just demonstrate it working through your unit tests.
Objectives

    Practice using the state pattern.

Requirements

    UML (30 pts)
        You should do your UML first before implementing
        You should submit a state diagram for each state machine
        You must use the state pattern
        Look for opportunities to use other design patterns where appropriate
    Implementation (30 pts)
        Each of your detectors should take a string and return a boolean that indicates whether or not the string matches the pattern that the detector was designed to detect.
        Your detector should take the input and split it into individual characters (or substrings that each represent a single character, I wouldn't use the Char datatype).
        Your states operate on a single character 
        Your program should be able to detect a valid integer
            An integer must start with a digit (1-9)
            An integer must not contain anything other than a digit (0 - 9)
            You can't use things like parseInt or toInt (you can't try to convert the string to an inte)
            Examples of valid integers include: "1", "123", "3452342352434534524346"
            Examples of invalid integers include: "" (empty string), "0123" (starts with a 0), "132a" (has something other than a digit in it), "0" (starts with a 0, even though this would actually be considered a valid integer in real life you can pretend it is not here)
        Your program should be able to detect a valid floating point value
            A floating point value can start with either a (1-9) or a 0, or a period (.)
            If the value starts with a 0 then a period must be the next character
            The period must be followed by at least one digit (0-9)
            A floating point value contains exactly one period 
            A floating point number must not contain any chars other than a period or a digit (0-9)
            Examples of valid floating point values include: "1.0", "123.34", "0.20000", "12349871234.12340981234098", ".123"
            Invalid floating point values include: "123" (no period), "123.123." (to many periods), "123.02a" (invalid char), "123." (nothing follows period), "012.4" (starting 0 is not followed by period)
        Your implementation must be able to detect a binary number that starts and ends with a 1
            Binary numbers only contain the digits (0,1) and no other characters
            Examples of valid numbers are, "1", "11", "101", "111111", "10011010001"
            Examples of invalid numbers are "01" (doesn't start with a 1), "10" (doesn't end with a 1), "1000010" (doesn't end with a 1), "100a01" (contains invalid char)
        Your implementation must be able to detect a valid email address
            An email address takes the form of <part1>@<part2>.<part3>
            An email address contains exactly 1 @ symbol, 
            An email address contains exactly 1 period after the @ symbol
            Neither part 1, 2, or 3 can be empty
            Any character other than the space character, or @ symbol are valid in part 1, 2, and 3 (rule 3 still applies for parts 2 and 3)
            Examples of valid email addresses are: "a@b.c", "joseph.ditton@usu.edu", "{}*$.&$*(@*$%&.*&*",
            Examples of invalid email addresses are: "@b.c" (part1 is empty), "a@b@c.com" (to many @ symbols), "a.b@b.b.c" (to many periods after the @), "joseph ditton@usu.edu" (space char not allowed)
        You should be able to detect a complex password
            A complex password is a password that has at least 1 capital letter, at least 1 special character (!@#$%&*), and cannot end with a special character.
            A password has at least 8 chars
                You should not use your state machine to count characters. It's possible and a fun exercise but once you start down that line you will see that it makes for a very, VERY complex state machine.
            Examples of valid passwords include: "aaaaH!aa", "1234567*9J", "asdpoihj;loikjasdf;ijp;lij2309jasd;lfkm20ij@aH"
            Examples of invalid passwords include: "a" (basically missing everything and to short), "aaaaaaa!" (no capital letter and ends with special char), "aaaHaaaaa" (no special char), "Abbbbbbb!" (ends with special char)
        Your implementation follows your design
    Unit Tests (20 pts)
        Each state machine should be thoroughly tested

Submit

You do 3 things when you submit. FAILURE to follow these instructions will result in a loss of points:

    Put your UML in the root of your project and zip up the project folder and submit that.
    Add a submission comment with a link to your github repository.
    Submit a video demonstration of your project (any normal video format is fine)

Video Demonstration

For the video demonstration you should do the following

    Tell us about your system from a high level.
    Show your UML. Highlight the design patterns you chose to apply and talk about what each pattern is used for in the software.
    Show your code. You don't need to show all of the code, instead show the implementations of the design patterns.
    Run your unit tests and show those.

This video does not need to be long (the shorter the better).