# Software Development Plan

## Phase 0: Requirements Specification *(10%)*

* Instructions in my own words
    * Due to a client that doesn't want to install linux and use the built-in text processing programs we have been hired to make those programs in python so that the client can easily use them to process large text files.
    * A solution needs to be simple so that the client can use the program with minimal training, and it should look like the old unix text processing programs that are also on linux.
* Things I already know how to do
  * I know how to read input from the command line and process it in my program.
  * I know how to redirect output to the file of my choosing.
  * I know how to interpret error messages and find how to fix them.
  * I know for this program that it's ok to let some error messages be thrown (for example when a user enters a invalid file).
* Things I anticipate as a challenge
  * Since my boss has already started working on this project but got busy with other matters it will be tedious to read through his work and pick up where he left off.
    
    
## Phase 1: System Analysis *(10%)*

Inputs taken by the program:
```
python3 src/tt.py TOOL [OPTIONS] FILENAME...
```

[ ] shows that this input type is optional.

The output of the program will depend on what tool the user selects. If the user misuses a tool then a message will be displayed to try and help them use the program.

####key function
def usage(error=None, tool=None):
input: what error happened and what tool it happened with.
output: prints a helper message.

Below is a list of all possible outputs. 

----------

###cat

When the User wants to use the cat tool, and they enter a valid file then the contents of the file will be printed out to the terminal. They can put in multiple file names and in the order entered they will be printed one after the other.

Input:
```
python3 src/tt.py cat data1.txt
```
Output:
```
This is example text
I like coding
```

####Invalid Input

If the user puts in a invalid file path then a default error message will be printed and the program will crash. If the user doesn't put in a valid number of files then a custom message will be displayed and the program will exit. You will know the difference because the error message will be look scary then the program will end. But with the custom message it looks like just some text being printed and then the program exiting peacefully.

input:
```
python3 src/tt.py cat data/complexit 
```
output:
```
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 50, in <module>
    cat(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/Concatenate.py", line 30, in cat
    file = open(filePath)
FileNotFoundError: [Errno 2] No such file or directory: 'data/complexit'
```

####key function
def cat(args):
input: the file names that the user entered.
output: prints the contents of the files.

----------

###tac

When the User wants to use the tac tool, and they enter a valid file then the contents of the file will be printed out in reversed order to the terminal. They can put in multiple file names and in the order entered they will be printed one after the other with the content of each file being in reverse order.

Input:
```
python3 src/tt.py tac data2.txt
```
Output:
```
I like coding
This is example text
```

####Invalid Input

Works the same as the cat tool.

####key function
def tac(args):
input: the file names that the user entered.
output: prints the contents of the files in reverse order.

----------

###paste

When the User wants to use the paste tool, and they enter a valid file then every line is printed with a newline and at the end of the line there is a comma that gets added. This is usful becuse if the user enters in two valid files then the first file will have all its lines outputted with a newline and then a comma at the end of each line, then the second file will have its lines put in after the commas from the first file so then it will look like a row and colum set up with commas and newlines to make the grid. This goes for any number of files the user puts in.

Input with one file:
```
python3 src/tt.py paste file1.txt 
```
Output:
```
1
2
3
4
5
6
```

Input with two files:
```
python3 src/tt.py paste file1.txt file2.txt
```
Output:
```
1,a
2,b
3,c
4,d
5,e
6,f
```
This pattern would go on for any number of files entered in.

Ok but now lets see how you could save the output to a new file and not print it out to the screen.

Input with three files and saving to a file:
```
python3 src/tt.py paste file1.txt file2.txt file3.txt > save.csv
```
Output:
```
```

Notice how the there is no output. But if we open up the save.csv file this is what it looks like. 

Input:
```
python3 src/tt.py cat save.csv
```
Output:
```
1,a,!
2,b,@
3,c,#
4,d,$
5,e,%
6,f,^
```

The .csv extension at the end of save stands for "Comma Separated Values". 

We moved the output from going onto the screen and moved it into a file, and now we have a grid system of sorts. 

####Invalid Input

Works the same as the cat tool.

####key function
def paste(args):
input: the file names that the user entered.
output: prints the contents of the files in a row and column format separated by commas and new lines.

----------

###cut

The reason why I explained the paste tool first is because paste makes data for cut to work with.

When the User wants to use the cut tool, and they enter a valid file then cut will extract fields of data. By default, the first field is printed with the rest being ignored.

Input:
```
python3 src/tt.py cut save.csv
```
Output:
```
1
2
3
4
5
6
```

If the user would like to get more than one field printed then they need to use the -f flag.

Input:
```
python3 src/tt.py cut -f 1,3 save.csv
```
Output:
```
1,!
2,@
3,#
4,$
5,%
6,^
```

Cut got the first and the third field values stored in the save.csv file and printed them out.

Another example of the -f flag being used

Input:
```
python3 src/tt.py cut -f 2 save.csv
```
Output:
```
a
b
c
d
e
f
```

When the user wants a out-of-order list the cut tool with print the fields as if the user inputs a in-order list

Input:
```
python3 src/tt.py cut -f 2,1 save.csv
```
Output:
```
1,a
2,b
3,c
4,d
5,e
6,f
```

Cut can process more then one file and each file is processed in the order that is given. Lets make a new CSV file called names.csv by using the paste tool.

Input:
```
python3 src/tt.py paste file4 file5 > names.cvs
```
Output:
```
```

For demonstration lets open up names.cvs

Input:
```
python3 src/tt.py cat names.cvs
```
Output:
```
Name,Age
Mark,20
John,90
Josh,21
Nate,40
Jake,50
```

Now lets use cut with the -f flag and then input both save.cvs and names.cvs

Input:
```
python3 src/tt.py cut -f 2 save.cvs names.cvs
```
Output:
```
a
b
c
d
e
f
Age
20
90
21
40
50
```

Notice now both files had there second feild outputed in the order the files where entered.

If the user puts in a field number that is greated then the number of feilds avalible then empty feilds are displayed.

Input:
```
python3 src/tt.py cut -f 19 save.cvs
```
Output:
```









```

Lets make a new file with paste and then cat the file to see what it looks like.

Input:
```
python3 src/tt.py paste file6 file7 > save1.cvs
```
Output:
```
```

Input:
```
python3 src/tt.py cat save1.cvs
```
Output:
```
Date,TODO
September 1,Eat
September 2,Sleep
           ,
           ,
September 5,Run
```

Notice how some fields are blank.

Let's cut the save1.cvs file 

Input:
```
python3 src/tt.py cut -f 2 save1.cvs
```
Output:
```
TODO
Eat
Sleep
Run




```

See the large amount of black lines created by this command. Some lines save a black 2ed field.

####Invalid Input

Works the same as the cat tool. But with one addition. Cut reports an error when the -f flag is not given any feilds.

Input:
```
python3 src/tt.py cut -f save1.cvs
```
Output:
```
Error: A comma-separated field specification is required

tt.py cut [-f LIST] FILENAME...
    Remove comma-separated sections from each line of files
    -f  List of comma-separated integers indicating fields to output
```

Any fields entered that are less than or equal to 0 will be ignored if all fields are less than or equal to 0 then the -f flag will act as if no feilds have been entered.

Input:
```
python3 src/tt.py cut -f -1 -7 save1.cvs
```
Output:
```
Error: A comma-separated field specification is required

tt.py cut [-f LIST] FILENAME...
    Remove comma-separated sections from each line of files
    -f  List of comma-separated integers indicating fields to output
```

####key function
def cut(args):
input: the file names that the user entered and the flag with its arguments if used.
output: prints the contents of the files in a row and column format separated by commas and new lines and the user can pick what data they would like to be shown.

----------

###grep

When the User wants to use the grep tool, they need to enter a pattern to search for. Then a valid file or many files to look in 

Input:
```
python3 src/tt.py grep Hey greetings.txt
```
Output:
```
Hey John
Hey Nate
Hey Kelsey
```

grep found all the lines in the given file or files that have the entered pattern and printed them.

Notice if the user looks for "hey" and not "Hey" there is no output. grep is case-sensitive.

Input:
```
python3 src/tt.py grep hey greetings.txt
```
Output:
```
```

The -v flag means to find lines that don't contain the given pattern.

Input:
```
python3 src/tt.py grep -v hey greetings.txt
```
Output:
```
Hey John
Hey Nate
Hey Kelsey
```

####Invalid Input

Works the same as the cat tool. But with some additions, if the user doesnt put in a pattern to search for then you get this.

Input:
```
python3 src/tt.py grep greetings.txt
```
Output:
```
Error: Please provide a pattern and at least one filename

tt.py grep [-v] PATTERN FILENAME...
    Print lines of files matching PATTERN
    -v  Invert matching; print lines which DO NOT match PATTERN
```

When no file is given.

Input:
```
python3 src/tt.py grep 
```
Output:
```
Error: Please provide a pattern and at least one filename

tt.py grep [-v] PATTERN FILENAME...
    Print lines of files matching PATTERN
    -v  Invert matching; print lines which DO NOT match PATTERN
```

####key function
def grep(args):
input: the file names that the user entered and the flag with its arguments if used.
output: prints the found patterns specified by the user.

----------

###head

When the User wants to use the head tool, and they enter a valid file then the first 10 lines get printed from a file. If the file is less then or equal to 10 lines then head acts just like cat.

Input:
```
python3 src/tt.py head data.txt
```
Output:
```
1
2
3
4
5
6
7
8
9
10
```

The user can specify the number of lines they would like to be printed from the file by using the -n flag.

Input:
```
python3 src/tt.py head -n 4 data.txt
```
Output:
```
1
2
3
4
```

The user can enter more than one file and a banner will be displayed so that the user knows what txt belongs to what file.

Input:
```
python3 src/tt.py head -n 5 data.txt data1.txt data2.txt
```
Output:
```
==> data.txt <==
1
2
3
4
5

==> data1.txt <==
a
b
c
d
e

==> data2.txt <==
Mark
Jack
John
Jake
Ben

```

####Invalid Input
Works the same as cat with one addition
If the user enters the -n flag but leaves out a number or if the number is spelled out then the following message will be displayed

Input:
```
python3 src/tt.py head -n data.txt
```
Output:
```
$ python src/tt.py head -n
Error: Number of lines is required

tt.py head|tail [-n N] FILENAME...
        Output the first or last part of files
        -n  Number of lines to print (default is 10)
```

####key function
def head(args):
input: the file names that the user entered and the flag with its arguments if used.
output: prints the number of lines from the top down to the specified line given by the user.

----------

###tail

When the User wants to use the tail tool, and they enter a valid file then the bottom 10 lines of the file are printed. 

Input:
```
python3 src/tt.py tail data.txt
```
Output:
```
1
2
3
4
5
6
7
8
9
10
```

Just like with the head tool the user can use the -n flag to specify how many lines they would like to see.

Input:
```
python3 src/tt.py tail -n 3 data.txt
```
Output:
```
1
2
3
```

When you combine the head and tail tool you can take a look at the middle of a file by utilizing the ">" redirection operator.

Input:
```
python3 src/tt.py head -n 8 data.txt > save.txt
```
Output:
```
```

Input:
```
python3 src/tt.py tail -n 5 save.txt
```
Output:
```
4
5
6
7
8
```

Just like with head when more than one file is specified then there will be a header declaring what outout belongs to what file.

Input:
```
python3 src/tt.py tail -n 5 data.txt data1.txt data2.txt
```
Output:
```
==> data.txt <==
6
7
8
9
10

==> data1.txt <==
f
g
h
i
j

==> data2.txt <==
Jen
Mike
Bill
Kate
Nate

```

####Invalid Input
Works the same as cat with one addition
If the user enters the -n flag but leaves out a number or if the number is spelled out then the following message will be displayed

Input:
```
python3 src/tt.py tail -n data.txt data1.txt data2.txt
```
Output:
```
Error: Too few arguments

tt.py head|tail [-n N] FILENAME...
        Output the first or last part of files
        -n  Number of lines to print (default is 10)
```

####key function
def tail(args):
input: the file names that the user entered and the flag with its arguments if used.
output: prints the number of lines from the bottom up to the specified line given by the user.

----------

###sort

When the User wants to use the sort tool, and they enter a valid file then the contents of the file will be sorted in lexical order. Lexical order is similar to alphabetical order but numbers are included in the ordering and upper case letters are considered to be higher in the order then lower case letters.

Input:
```
python3 src/tt.py sort sortme.txt
```
Output:
```
Antique White
Dark Goldenrod
DarkSea Green
Dodger Blue
Favorite Color
Light Salmon
Midnight Blue
Royal Blue
Snow
```

A user can enter more than one file and the ordering won't change based off of what order the files are entered in.

Input:
```
python src/tt.py sort colors8 names10
```
Output:
```
Alexa
Angela
Antique White
Bailey
Dark Goldenrod
DarkSea Green
Dodger Blue
Favorite Color
Frank
Hazel
Isabel
Jerry
Kai
Karen
Light Salmon
Midnight Blue
Mikayla
Royal Blue
Snow
```

If a user would like to have a file in reverse sorting order then they can use the tac tool in combination with saving the output to a file.

Input:
```
python src/tt.py sort colors8 names10 > save.txt
```
Output:
```
```

Input:
```
python src/tt.py tac save.txt
```
Output:
```
Snow
Royal Blue
Mikayla
Midnight Blue
Light Salmon
Karen
Kai
Jerry
Isabel
Hazel
Frank
Favorite Color
Dodger Blue
DarkSea Green
Dark Goldenrod
Bailey
Antique White
Angela
Alexa
```

####Invalid Input
Works the same as cat with one addition if no files are inputted to sort then a message will be displayed.

Input:
```
python src/tt.py sort
```
Output:
```
Error: Too few arguments

tt.py sort FILENAME...
        Output lines of text file in sorted order
```

####key function
def sort(args):
input: the file names that the user entered.
output: sorts the contents of the given files in lexical order and prints the result.

----------

###wc

When the User wants to use the wc tool, and they enter a valid file then the number of lines, number of words, and number of charters in the file are shown in a grid.

Input:
```
python3 src/tt.py wc num2
```
Output:
```
2   2   4   num2
```

A user can enter more than one file and the total for everything will be displayed at the end.

Input:
```
python3 src/tt.py wc let3 random20 words200 dup5 complexity
```
Output:
```
     3       3       6  data/let3
    20      20      51  data/random20
   200     200    1790  data/words200
     8       8      16  data/dup5
     9      41     289  data/complexity
   240     272    2152  total
```

A user can enter the same file many times and the wc tool won't care.

Input:
```
python3 src/tt.py wc let3 let3 let3
```
Output:
```
     3       3       6  data/let3
     3       3       6  data/let3
     3       3       6  data/let3
     9       9      18  total
```

####Invalid Input
Works the same as cat with one addition if no files are inputted to sort then a message will be displayed.

Input:
```
python3 src/tt.py wc 
```
Output:
```
Error: Too few arguments

tt.py wc FILENAME...
    Print newline, word, and byte counts for each file
```

####key function
def wc(args):
input: the file names that the user entered.
output: prints the number of lines, the word count, and the number of characters in the file.

----------



## Phase 2: Design *(30%)*

The program will start by running the tt.py program in the terminal and will be followed by what tool the user wants to use and additional arguments if the tool utilizes them and the files that the user wants the tool to play with.
* tt.py
  * import the correct functions from the other files used in the program.
  * check if the user has enough arguments entered. If they don't then show a helper message.
  * check what tool the user wants to use by looking at the first argument entered the program.
  * after tool has been found the check to see if there are enough arguments to be passed into the tools function if there isn't then show a usage message. If there is enough arguments then pass the rest of the data into the tools function.
* Usage.py
  * print the error message passed in.
  * find the correct message to be displayed based on what tool the user is trying to use.
  * exit the program with error code 1
* Helper.py
  * print helpful message
  * exit the program with error code 2
* Concatenate.py
  * def cat(args):
    * get every file path from the args passed in and get a file object out of the path. The program could crash at this point if a non-valid path was given.
    * get every line out of the file object and print out the line without a newline at the end of the print because there is already one in the line.
    * close the file object and end program.
  * def tac(args):
    * get every file path from the args passed in and get a file object out of the path. The program could crash at this point if a non-valid path was given.
    * get every line out of the list of lines from the file object with the last line first and print the line without a newline at the end of the print because there is already one in the line.
    * close the file object and end program.
* CutPaste.py
  * def cut(args):  
    * check to see if the -f flag is used.
    * if it is used then split up the numbers used after the flag by the comma and save that in a listOfWhatRows default first element in list is 1 but that would be overwritten, if there is no numbers after then use the usage message.
    * check to see how many filenames are after the flag if flag is used.
    * if there is one then get a file object out of the filename and get a list of lines from the file object. split the line-up by comma separation and save the result in a seperatedList.
    * print the seperatedList at the listOfWhatRows first element and then the second and so on, do this in a loop. close the file object after printing is done.
    * if there is more than one filename in the list of arguments then iterate over each filename and repeat past steps to get all the data printed as needed.
  * def paste(args):  	
    * check to see how many filenames are in the list of arguments.
    * if there is one then get a file object out of the filename. then get a list of all the lines in that file object. iterate over each line and print the line out with a comma at the end of it. once all the lines have been accounted for then close the file object.
    * if there is more than one then get the first filename and get a file object and get a list of all the lines in that file object. 
    * iterate over the rest of the filenames in the argument list. with the second filename make a file object and then get a list of all the lines in the file object. add the first line of the second file object to the first line of the first file object with a comma between the lines. repeat for all lines. close file object.
    * repeat for all filenames.
    * print the list with all the additions and commas.
* Grep.py
  * def grep(args):  
    * check to see if the -v flag was used by checking if the first element in the argument list is equal to -v.
    * check to see how many filenames are after the flag if the flag is used. 
    * if there is one then get a file object out of the filename and then get a list of lines from the file object.
    * iterate over each line in the list of lines and use pythons "in" tool and test for if "what the user wants to find" in "the list of lines" then print out that line or if the user used the -v flag on the pattern then if a line that the pattern is found then don't print it. close the file object.
    * If there is more than one then iterate over each filename and repeat past steps to find what lines have the pattern the user wants to find. 
* Partial.py
  * def head(args):  	
    * check to see if the -n flag was used by checking if the first element in the argument list is equal to -n. If it is then find the second argument and check to see if there is a number in that position. If there is then save that variable for later use. If there isn't a number than call the usage function.
    * check to see how many arguments there are after the -n flag and the number argument after the flag if it's used. If there is one argument after the flag then grab the filename from the list of arguments and turn that filename into an object.
    * get a list of all the lines in the file and grab one line at a time. print the lines until the amount of lines printed is equal to the -n argument saved earlier (10 by default).
    * if there is more than one filename in the list of arguments then make a banner for the first filename and print it out. then make a file object out of the first filename and repeat the step above this one. after done then close the file object. do this until all the filenames have been used. 
  * def tail(args):  
    * check to see if the -n flag was used by checking if the first element in the argument list is equal to -n. If it is then find the second argument and check to see if there is a number in that position. If there is then save that variable for later use. If there isn't a number than call the usage function.
    * check to see how many arguments there are after the -n flag and the number argument after the flag if it's used. If there is one argument after the flag then grab the filename from the list of arguments and turn that filename into an object.
    * get a list of all the lines in the file object make a loop and get the last line then the second to last and so on. print the lines until the amount of lines printed is equal to the -n argument saved earlier (10 by default).
    * if there is more than one filename in the list of arguments then make a banner for the first filename and print it out. then make a file object out of the first filename and repeat the step above this one. after done then close the file object. do this until all the filenames have been used.
* Sorting.py
  * def sort(args):  	
    * Check to see how many filenames are in the argument list.
    * if there is one then grab the filename out of the argument list and turn that into a file object. get a list of lines from the file object and call the built-in sort function to get all the lines in the list sorted. 
    * iterate over each line in the list and print it out then close the file object. 
    * If there is more than one file in the list then iterate over each filename in the argument list. turn the filename into a file object then get a list of all the lines in the file object. get the next filename get a file object out of that and add all the lines in that file to the list from the first file. repeat this until all the filenames have been used. 
    * close each file object when the lines have been added to the list.
    * now there should be a list of all the lines in all the files. use the built-in sort function that python has and then iterate over each line in the list and print it out.
* WordCount.py
  * def wc(files):  	
    * check how many filenames are in the argument list. 
    * if there is one then grab the filename out of the argument list and turn that into a file object. get a list of lines from the file object and check how many items are in list and that number is the number of lines in the file. 
    * Then iterate over each line in the list and split up each item in the line by the space character and save that to a new word list and check how many items are in that list and save that to a wordCount variable then iterate over the new word list and grab one word at a time. 
    * Check how big each word is and save that number to a charCount variable. check the next word and add its length to the charCount. 
    * When on the next line in the list add the number of words to the wordCount variable. repeat until all lines have been checked.
    * print out stats in lineCount wordCount charCount filename format.
    * close file object
    * If there is more than one file in the argument list then iterate over each filename and repeat past steps to get stats for that file. then print the totals for everything.
    
## Phase 3: Implementation *(15%)*

All my pseudocode worked for every function and file up until the CutPaste.py file. The cut and paste tools and therefor there functions where harder to implement then I thought they would be. I needed to be much more careful how I managed the lists of lines coming in. I learned a lot of cool python shortcuts when it comes to playing with lists and strings that fixed my issues with implementing the CutPaste.py code. But I think some of my biggest issues where when python would assume too much. This is the reason I like java much better because you get you tell it exactly what you want, and it doesn't assume anything. I wish this class was in java for that reason, but I understand why you guys choice python for this class.


## Phase 4: Testing & Debugging *(30%)*

### Good and bad input with the corresponding output

--------------

#### cat

##### Good

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py cat data/complexity 
```
output:
```
     "There are two ways of constructing a software design:

                one way is to make it so simple
           that there are obviously no deficiencies,

           and the other is to make it so complicated
            that there are no obvious deficiencies."

    -- Tony Hoare
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py cat data/complexity data/debugging 
```
output:
```
     "There are two ways of constructing a software design:

                one way is to make it so simple
           that there are obviously no deficiencies,

           and the other is to make it so complicated
            that there are no obvious deficiencies."

    -- Tony Hoare
        "Everyone knows that debugging is twice as hard
            as writing a program in the first place.

    So if you're as clever as you can be when you write it,
                  how will you ever debug it?"

    -- Brian Kernighan
```

##### Bad

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py cat data/complexit 
```
output:
```
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 50, in <module>
    cat(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/Concatenate.py", line 30, in cat
    file = open(filePath)
FileNotFoundError: [Errno 2] No such file or directory: 'data/complexit'
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py cat
```
output:
```
Error: Too few arguments

	tt.py cat|tac FILENAME...  	    	       
    Concatenate and print files in order or in reverse
```

I forgot to add code to check if there were no arguments passed after the tool, so I added that.

--------------

### tac

##### Good

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py tac data/complexity
```
output:
```
    -- Tony Hoare

            that there are no obvious deficiencies."
           and the other is to make it so complicated

           that there are obviously no deficiencies,
                one way is to make it so simple

     "There are two ways of constructing a software design:
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py tac data/complexity data/debugging 
```
output:
```
    -- Tony Hoare

            that there are no obvious deficiencies."
           and the other is to make it so complicated

           that there are obviously no deficiencies,
                one way is to make it so simple

     "There are two ways of constructing a software design:
    -- Brian Kernighan

                  how will you ever debug it?"
    So if you're as clever as you can be when you write it,

            as writing a program in the first place.
        "Everyone knows that debugging is twice as hard
```

##### Bad

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py tac data/complexity data/debuggin 
```
output:
```
    -- Tony Hoare

            that there are no obvious deficiencies."
           and the other is to make it so complicated

           that there are obviously no deficiencies,
                one way is to make it so simple

     "There are two ways of constructing a software design:
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 54, in <module>
    tac(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/Concatenate.py", line 42, in tac
    file = open(filePath)
FileNotFoundError: [Errno 2] No such file or directory: 'data/debuggin'
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py tac                              
```
output:
```
Error: Too few arguments

	tt.py cat|tac FILENAME...  	    	       
    Concatenate and print files in order or in reverse
```

--------------

### cut

##### Good

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py cut data/PeoplesInformation.csv 
```
output:
```
Name
Adrianna
Julian
Tiffany
Savannah
Abraham
Michael
Marcus
Julianna
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py cut -f 4,2 data/PeoplesInformation.csv
```
output:
```
Age,Locomotion Style
22,crawl
36,traipse
24,push
39,march
26,trot
23,lurch
29,slink
17,wriggle
```

##### Bad

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py cut data/ag    
```
output:
```
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 62, in <module>
    cut(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/CutPaste.py", line 54, in cut
    fileObject = open(fileName)
FileNotFoundError: [Errno 2] No such file or directory: 'data/ag'
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py cut
```
output:
```
Error: Too few arguments

	tt.py cut [-f LIST] FILENAME...  	    	       
    Remove comma-separated sections from each line of files  	    	       
    -f  List of comma-separated integers indicating fields to output (default is LIST=1)
```

--------------

### paste

##### Good

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py paste data/names8 data/ages8 data/colors8 data/verbs8 
```
output:
```
Name,Age,Favorite Color,Locomotion Style
Adrianna,22,Royal Blue,crawl
Julian,36,Midnight Blue,traipse
Tiffany,24,Light Salmon,push
Savannah,39,Antique White,march
Abraham,26,DarkSea Green,trot
Michael,23,Dodger Blue,lurch
Marcus,29,Dark Goldenrod,slink
Julianna,17,Snow,wriggle
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py paste data/names8 data/ages8 data/colors8 data/verbs8 > data/PeoplesInformation.csv
```
output:
```
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py paste data/num2 data/names10 data/dup5        
```
output:
```
1,Jerry,1
2,Bailey,1
,Frank,2
,Kai,2
,Angela,3
,Mikayla,4
,Hazel,4
,Karen,5
,Alexa
,Isabel
```

##### Bad

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py paste data/num2 data/nam         
```
output:
``` 
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 58, in <module>
    paste(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/CutPaste.py", line 68, in paste
    fileObject = open(fileName)
FileNotFoundError: [Errno 2] No such file or directory: 'data/nam'
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py paste                   
```
output:
```
Error: Too few arguments

	tt.py paste FILENAME...  	    	       
    Merge lines of files into one comma-separated text
```

--------------

### grep

##### Good

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py grep ing data/words200
```
output:
```
uttering
bequeathing
enlightening
belaying
stapling
starling
ping
broiling
alternating
tossing
rebelling
inhabiting
ridding
flooding
underlining
interpolating
climbing
airing
inklings
commissioning
convincing
convicting
exacerbating
latching
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py grep -v a data/words200
```
output:
```
implicitly
opponents
clubroom
uttering
roughen
sleighs
smelt
reminder
inhibitions
onsets
wedge
recomputed
dined
southbound
cleverly
your
consisted
repulsion
willed
enlightening
flocked
succumbs
indirections
secreted
finders
finders
syrup
deleter
ping
gunmen
broiling
burly
strut
tired
tossing
flees
rebelling
reregister
exported
mounded
oxide
sterilizes
hydrogen
even
outperforms
lustiness
boom
improviser
the
restive
ridding
monotonous
sincere
dignify
unrecognized
conjunct
hemp
best
foresters
flooding
reminiscences
weepers
weepers
revoker
keepers
keepers
willows
person
bite
underlining
suspended
preys
invest
climbing
inklings
smokescreen
renewer
losers
losers
purifies
commissioning
overly
deterministic
overthrown
bitterly
courtly
progressions
otters
convincing
rounded
revenge
lied
declines
remotest
first
shocker
plies
robotic
chucks
convicting
indictment
very
```

I forgot to tell the print function to not print a newline char because there is already one in the line.

##### Bad

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py grep -v v data/wor     
```
output:
```
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 66, in <module>
    grep(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/Grep.py", line 40, in grep
    fileObject = open(fileName)
FileNotFoundError: [Errno 2] No such file or directory: 'data/wor'
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py grep              
```
output:
```
Error: Please provide a pattern and at least one filename

	tt.py grep [-v] PATTERN FILENAME...  	    	       
    Print lines of files matching PATTERN  	    	       
    -v  Invert matching; print lines which DO NOT match PATTERN
```

--------------

### head

##### Good

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py head data/let3                        
```
output:
```
a
b
c
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py head data/let3 data/names10
```
output:
```
==> data/let3 <==
a
b
c
==> data/names10 <==
Jerry
Bailey
Frank
Kai
Angela
Mikayla
Hazel
Karen
Alexa
Isabel
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py head -n 2 data/let3      
```
output:
```
a
b
```

I needed to change the print statement that prints the header for the file to print a newline char.

##### Bad

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py head data/let3 data/jdkl;af
```
output:
```
==> data/let3 <==
a
b
c
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 70, in <module>
    head(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/Partial.py", line 32, in head
    printData(howManyFileNames, listOfFileNames, numOfLines, "head")
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/Partial.py", line 37, in printData
    fileObject = open(fileName)
FileNotFoundError: [Errno 2] No such file or directory: 'data/jdkl'
zsh: command not found: af
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py head                       
```
output:
```
Error: Too few arguments

	tt.py head|tail [-n N] FILENAME...  	    	       
    Output the first or last part of files  	    	       
    -n  Number of lines to print (default is N=10)
```

--------------

### tail

##### Good

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py tail data/let3 data/names10
```
output:
```
==> data/let3 <==
a
b
c
==> data/names10 <==
Jerry
Bailey
Frank
Kai
Angela
Mikayla
Hazel
Karen
Alexa
Isabel
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py tail -n 2 data/let3 data/names10
```
output:
```
==> data/let3 <==
b
c
==> data/names10 <==
Alexa
Isabel
```

##### Bad

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py tail -n 2 data/le               
```
output:
```
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 74, in <module>
    tail(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/Partial.py", line 87, in tail
    printData(howManyFileNames, listOfFileNames, numOfLines, "tail")
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/Partial.py", line 47, in printData
    fileObject = open(fileName)
FileNotFoundError: [Errno 2] No such file or directory: 'data/le'
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py tail -n
```
output:
```
Error: Too few arguments

	tt.py head|tail [-n N] FILENAME...  	    	       
    Output the first or last part of files  	    	       
    -n  Number of lines to print (default is N=10)
```

forgot to check if there were no arguments after the -n flag

--------------

### sort

##### Good

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py sort data/words200
```
output:
```
abreast
absorb
abstainer
accounted
activates
adumbrates
adverbs
aim
airing
alternating
alveolus
anesthetics
announced
appeared
arbitrary
aridity
arraigned
autobiographies
awakes
axiological
backtracked
bark
beater
beaus
becalm
befall
belaying
bequeathing
best
bite
bitterly
boom
brawn
braziers
briefcase
broiling
burglarproofed
burly
calibers
capitalism
captivity
chandeliers
characterizers
chrysanthemum
chucks
cleverly
climbing
clubroom
cocktail
commissioning
concomitant
conjunct
consisted
convicting
convincing
courtly
cranberry
cranky
dabbled
dad
declines
deleter
deterministic
dignify
dined
dislocated
draughts
easiest
easter
enlightening
even
exacerbating
exclaims
exported
finders
finders
first
flees
flocked
flooding
foresters
gantry
garbed
gunmen
handicap
harrows
hemp
hierarchic
honoraries
hydrogen
impersonated
implicitly
improviser
increased
indictment
indirections
inferential
inhabiting
inhibitions
inklings
insomniac
instances
interpolating
invest
keepers
keepers
keyboard
large
latching
lava
lied
losers
losers
lustiness
malts
monotonous
mounded
musicals
obligations
onsets
opponents
otters
outperforms
overall
overly
overthrown
oxide
parse
parsimony
pastness
person
ping
plastered
plies
preys
primaries
principals
progressions
proliferate
pulsate
purifies
randomizes
rash
rebelling
recomputed
reconfigurations
reloaded
reminder
reminiscences
remotest
renewer
replicates
repulsion
reregister
restive
revenge
revoker
ridding
robotic
roughen
rounded
scaly
scrap
secreted
shameless
shocker
sincere
sleighs
smelt
smokescreen
social
southbound
stalls
stapling
starling
sterilizes
strut
subprogram
succumbs
suffocated
suspended
switchboard
syrup
the
tired
tossing
underlining
underwear
unrecognized
upland
uttering
very
wants
weaknesses
wedge
weepers
weepers
willed
willows
your
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py sort data/names10 data/verbs8 
```
output:
```
Alexa
Angela
Bailey
Frank
Hazel
Isabel
Jerry
Kai
Karen
Locomotion Style
Mikayla
crawl
lurch
march
push
slink
traipse
trot
wriggle
```

##### Bad

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py sort data/na                 
```
output:
```
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 78, in <module>
    sort(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/Sorting.py", line 34, in sort
    fileObject = open(fileName)
FileNotFoundError: [Errno 2] No such file or directory: 'data/na'
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py sort        
```
output:
```
Error: Too few arguments

	tt.py sort FILENAME...  	    	       
    Output lines of text file in sorted order
```

--------------

### wc

##### Good

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py wc data/words200
```
output:
```
     200      200     1590   data/words200
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py wc data/words200 data/complexity 
```
output:
```
     200      200     1590   data/words200
       9       41      186   data/complexity
     209      241     1776   totals
```

##### Bad

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py wc data/wo                      
```
output:
```
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/tt.py", line 82, in <module>
    wc(sys.argv[2:])
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn2/src/WordCount.py", line 55, in wc
    fileObject = open(fileName)
FileNotFoundError: [Errno 2] No such file or directory: 'data/wo'
```

input:
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn2 % python3 src/tt.py wc 
```
output:
```
Error: Too few arguments

	tt.py wc FILENAME...  	    	       
    Print newline, word, and byte counts for each file
```

I needed to check if there were no filenames being passed in

--------------


## Phase 5: Deployment *(5%)*

*   *Important:* complete **Phase 6** first!
    *   (I know it's backwards, just go with it)
*   **YOU DON'T NEED TO WRITE ANYTHING IN THIS PHASE**
    *   Just follow this checklist
*   **Push** your final commit to GitLab
*   **Verify** that your final commit was received by *browsing* to its project page on GitLab
    *   Ensure the project's *URL is correct*
    *   Review that all required files are present *in the correct location*
    *   Check that unwanted files *have not* been committed
    *   Add *final touches* to your documentation, including the Sprint Signature and this Plan
*   **Validate** that your submission is complete and correct by *cloning* it to a new location on your computer and re-running it
	*	Run your program from the *command line* so you can see how it will behave when your grader runs it
        *   **Testing in PyCharm is not good enough!**
    *   Re-run your *test cases* to avoid nasty surprises

  
## Phase 6: Maintenance

Write *brief and honest* answers to these questions: *(Note: do this before you complete **Phase 5: Deployment**)*
  * ok

What parts of your program are *sloppily written* and *hard to understand*?
  * I don't think any of them are sloppily written because I made a plan, and I stuck with it for the most part so there is organization in my code. Also, all the functions that we had to write for this assigment had a lot of commonality at least in the beginning of the tools function so there is structure and a pattern.

Are there parts of your program which you *aren't quite sure* how/why they work?
  * I wouldn't use it if it didn't make sense so no there isn't any parts that don't make sense. but I did have to learn some new python tricks to get some things to work.

If a bug is reported in a few months, *how long would it take you to find the cause*?
  * Very fast because the program isn't that big I would just need to throw on the debugger in pycharm and I could fix it quick.

Will your documentation make sense to...


  ...anybody *besides yourself*? 
  * yes it's not a big thing of code

  ...*yourself* in six month's time? 
  * yes it's not a big thing of code

How easy will it be to *add a new feature* to this program in a year? 
  * pretty easy because I would just need to add some methods and tie everything together to add new features.

Will your program *continue to work* after upgrading...


...your computer's *hardware*?
  * yes because python works on almost any computer

...the *operating system*?
  * yes because python works on almost any computer

...to the *next version* of Python?
  * yes because they rarely get rid of things with updates they normally just add features.

Fill out the *Assignment Reflection* survey on Canvas
  * ok




