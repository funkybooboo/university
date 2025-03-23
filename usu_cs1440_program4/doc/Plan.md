# Software Development Plan

## Phase 0: Requirements Specification *(10%)*

We are making a classic Bingo card generator. The user will be able through a interactive menu pick what kind of bingo card they want to be created. They can pick a **N by N** card where **3 <= N <= 16**. If the card size picked by the user is even then there will be no *FREE!* space in the middle of the board sense there is no middle. But odd number sizes will have a *FREE!* space in the middle. The numbers on the cards will be unique with numbers on columns increasing from left to right and the range of numbers don't overlap.

There will be two menus that the user can use to interact with the program. 
* A Main menu that will let the user pick to make a new card or to quit the program.
* A Deck menu that will let the user view created cards, save the deck to a file, or go back to the Main menu.

When the program is given invalid input on either of the menus then a message will be printed about what went wrong and then the user can try to give input again. This process continues until valid input is given. The menus are case-insensitive meaning the menus will treat an uppercase letter the same as a lowercase letter.

The program structure was designed before the unit tests where created but these tests will ensure that the programs' implementation will follow the design and the tests will pass upon completion of that goal.

A good solution should have a intuitive user interface where it takes little key strokes to get the program to do that the user wants. As well as it should be robust and not crash at any point unless the user puts in a invalid file path for saving the deck to a file since management at DuckieCorp has instructed to let the program crash with pythons built in crash message if this where to happen.

#### Things I know how to do:
* Display menus and prompts to the user 
* take and check user input for valid types of input
  * reprint a prompt when a user puts in invalid input
* convert stings to integers
* Print a card to the customers specifications via string manipulation
* Check the user card size to check if there should be a *FREE!* spot in the middle of the card
* print output to a file instead of the console
* use unit tests
* pick numbers for the bingo cards such that 
  * numbers on the card are unique
  * numbers increase from left to right on columns
  * make numbers random

#### Things I don't know how to do:
* Write unit tests

## Phase 1: System Analysis *(10%)*

A menu will appear when the user first runs the program, and it will look like this:

```

 ########   ####  ##    ##   ######     #######   ####
 ##     ##   ##   ###   ##  ##    ##   ##     ##  ####
 ##     ##   ##   ####  ##  ##         ##     ##  ####
 ########    ##   ## ## ##  ##   ####  ##     ##   ##
 ##     ##   ##   ##  ####  ##    ##   ##     ##
 ##     ##   ##   ##   ###  ##    ##   ##     ##  ####
 ########   ####  ##    ##   ######     #######   ####

    Welcome to the DuckieCorp Bingo! Deck Generator

Main menu:
C) Create a new deck
X) Exit the program

Enter a Main command (C, X)

```

The user can then make a choice to exit the program or create a new deck.

Create a new deck:

The user will be asked questions about how big should the cards be? i.e. 3 - 16 inclusive. and how many cards should be created in the deck. Numbers on the cards will increase from left to right from as low as 1 to the max M can be as low as M = (2 * N^2), or as great as M = (floor(3.9 * N^2)).

After the cards are created the user will be provided more options. Namely, a choice to look at the created cards, save the deck to a file, or go back to the main menu.

If a user wants to look at the created cards then the deck will be printed out to the console. Cards will be created with numbers, +, -, and | characters.

If a user wants to save the deck to a file then they will be asked for a file path of where to save the deck.

If the user wants to go back to the main menu then the current created deck will be discarded and the Main menu prompt will be put onto the console.

Exit the program:

The program will end with exit code 0.

#### Algorithms and Formulae:

To make sure that we get random numbers that don't repeat in our cards we will create a list and store the already used numbers from pythons built in random number generator and only user numbers if they are not in the list. Random numbers will be created from (1,M) where M can be as low as M = (2 * N^2), or as great as M = (floor(3.9 * N^2)).

The requirement that columns of the card contain non-overlapping ranges of numbers can be achieved by applying the following idea N times, with the calculated random set of numbers that doesn't have any repeated numbers the first column gets the first 1/n of numbers, then second column gets numbers in the 2/n range, the third column 3/n, etc.


## Phase 2: Design *(30%)*

* MenuOption.py
  * def __init__(self, chCommand, szDescription):
    * figure out if the user put in a char option for the menu item
    * if they did they make the char option for this object be equal to whatever they entered such as "A".
    * if they didn't then make the char option be "?"
    * Find out if the user put in a phase to describe the char option. If they did then make the phase of this object equal to the phrase they phased in.
    * if they didn't put in a phrase to describe the char option the make the phrase equal to "???".
  * def chCommand(self):  
    * return the char option.
  * def szDescription(self):
    * return the describing phrase.
  * def __str__(self): 
    * return the char option + ")" + the description phrase.
* Menu.py
  * def __init__(self, szHeader):
    * make the header passed be the header of the object
    * make a options list
  * def __iadd__(self, option):
    * add whatever the option passed in to the options list
    * return the object
  * def __getitem__(self, nIdx):
    * Get the index of the list of the nIdx number passed in
  * def __len__(self):  
    * return the length of the options list
  * def bIsValidCommand(self, chCommand): 
    * check if the option passed in is valid. if it is then return true is its not then return false
  * def prompt(self):
    * print the header 
    * print the menu options with their phrases.
    * Check if there is valid input by the user with the bIsValidCommand() method
    * if its not valid then print out the menu again
    * if it is valid then return the command
* UserInterface.py
  * def __init__(self):  
    * create a starting Main menu
  * def run(self):
    * get the user input and direct user command to action
  * def __create_deck(self): 
    * get user input about how big the cards should be and how many cards there should be and what should be the max number on the cards and then conferm that they gave valid input then create a deck object and call deck menu
  * def __deck_menu(self):  	
    * make a deck menu with the menu and menu option classes
    * get the user input and direct the users command to an action
  * def __get_str(self, prompt):
    * get the input with the prompt of the input being the prompt passed in to the method
    * check if what the user put int is valid input
    * valid input is a non-empty string. 
    * return the string
  * def __get_int(self, prompt, lo, hi):  
    * get the input with the prompt of the input being the prompt passed in to the method
    * check to see if the user put a number in that is in between lo and hi.
    * return the number
  * def __print_card(self): 
    * Access a card from the current deck and print it out by finding its ID ie its index in the list of cards in the deck object
  * def __save_deck(self):
    * save the deck to a file by printing out one card at a time in the file.
* RandNumberSet.py
  * def __init__(self, nSize, nMax):
    * make a list with continually larger and larger numbers and check if the number is already used before you put them in the list
  * def shuffle(self):  
    * get each list in the segments 2d list and shuffle the numbers in the list
  * def next_row(self): 
    * populate the row with numbers
  * def get_segments(self): 
    * return the 2d list of lists with numbers 
  * def __str__(self):  
    * get all the lists in the 2d list and put them in a new list and return a newline char and the new list
  * def __len__(self):  
    * return the size of the card
  * def __getitem__(self, n):  	
    * return each a list of all the rows of numbers
* Deck.py
  * def __init__(self, card_size, num_cards, max_num):
    * Make a list of cards in the deck
  * def __len__(self):
    * return the number of cards in the deck
  * def __getitem__(self, n):
    * return the card at the number the user puts in
  * def __str__(self):  	
    * return the string of each card in the list and put the card number at the top
* Card.py
  * def __init__(self, idnum, ns):
    * Shuffle the ns (RandNumberSet) class 
    * make a 2d list that holds all the numbers for the card
    * make the middle of the card equal to FREE! if the card size is even
  * def id(self):
    * return the idnum passed into the object
  * def number_at(self, row, col):
    * return the 2d array at the row and col indexes
  * def __len__(self):
    * return the len(ns) passed into the object
  * def __str__(self):
    * add the row of column names to the first four positions
    * make a row separator
    * make two loops and go over each cell in the card and grab a number out of the 2d list of numbers with every go. Put a | between every cell.
  * def __get_cell_padding(self, n):
    * for a numbers size get the amount of spaces that there should be in the cell
  * def __make_row_sep(self):
  * Make a whole line of row separators

## Phase 3: Implementation *(15%)*

I thought everything went smoothly. The code really isn't that card its just figuring out how everything works together that took time. Overall fun time writing the code and my pseudocode really helped me understand what is going on. 

There was a bug in the RandNumberSet.py file and call I had to do is when the numbers where being added to the segment list it was adding one to the number and then saving it in the list. After I got rid of the plus one, so it was just saving the number my code started to not have any duplicate numbers.

I also needed to add some logic in the Deck.py that if an index outside the card IDs was passed in then I should return None.

I also simplified some of my code in the UserInterface.py and make getting input from the user much easier to read.


## Phase 4: Testing & Debugging *(30%)*

Had to make some minor changes but overall everything was working great. Looking at the python docs really helped.

Here is a large test I did with my code. Everything seems to be working great. I'll add some comments in with the copied input and output from my terminal running the bingo program.
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn4 % python3 src/bingo.py

 ########   ####  ##    ##   ######     #######   ####
 ##     ##   ##   ###   ##  ##    ##   ##     ##  ####
 ##     ##   ##   ####  ##  ##         ##     ##  ####
 ########    ##   ## ## ##  ##   ####  ##     ##   ##
 ##     ##   ##   ##  ####  ##    ##   ##     ##
 ##     ##   ##   ##   ###  ##    ##   ##     ##  ####
 ########   ####  ##    ##   ######     #######   ####

    Welcome to the DuckieCorp Bingo! Deck Generator

Main menu:
C) Create a new deck
X) Exit the program

Enter a Main command (C, X)
j
'j' is not a valid option

Main menu:
C) Create a new deck
X) Exit the program

Enter a Main command (C, X)
3
'3' is not a valid option

Main menu:
C) Create a new deck
X) Exit the program

Enter a Main command (C, X)
x
```
At this point I have tested that the menu asks for valid input in the face of bad input
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn4 % python3 src/bingo.py

 ########   ####  ##    ##   ######     #######   ####
 ##     ##   ##   ###   ##  ##    ##   ##     ##  ####
 ##     ##   ##   ####  ##  ##         ##     ##  ####
 ########    ##   ## ## ##  ##   ####  ##     ##   ##
 ##     ##   ##   ##  ####  ##    ##   ##     ##
 ##     ##   ##   ##   ###  ##    ##   ##     ##  ####
 ########   ####  ##    ##   ######     #######   ####

    Welcome to the DuckieCorp Bingo! Deck Generator

Main menu:
C) Create a new deck
X) Exit the program

Enter a Main command (C, X)
c
What should the card size be? i.e. [3-16]: 2
What should the card size be? i.e. [3-16]: 17
What should the card size be? i.e. [3-16]: 9000
What should the card size be? i.e. [3-16]: 3
How many cards should there be in the deck? i.e. [1-8192]: 0
How many cards should there be in the deck? i.e. [1-8192]: 8193
How many cards should there be in the deck? i.e. [1-8192]: 3
What should the max number be on the cards? i.e. [12-999]: 11
What should the max number be on the cards? i.e. [12-999]: 1000
What should the max number be on the cards? i.e. [12-999]: 20
```
At this point I have shown that the program asks for valid input over and over again while making the deck
```
Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
p
What card?: -1
What card?: 3
What card?: 1

Card #1
   B     I     N  
+-----+-----+-----+
| 1   | 9   | 15  |
+-----+-----+-----+
| 7   |FREE!| 21  |
+-----+-----+-----+
| 5   | 14  | 18  |
+-----+-----+-----+
```
At this point I have shown that the program will only print cards that exist in the deck
```


Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
d
Card #0
   B     I     N  
+-----+-----+-----+
| 5   | 12  | 18  |
+-----+-----+-----+
| 6   |FREE!| 21  |
+-----+-----+-----+
| 7   | 11  | 15  |
+-----+-----+-----+

Card #1
   B     I     N  
+-----+-----+-----+
| 1   | 9   | 15  |
+-----+-----+-----+
| 7   |FREE!| 21  |
+-----+-----+-----+
| 5   | 14  | 18  |
+-----+-----+-----+

Card #2
   B     I     N  
+-----+-----+-----+
| 4   | 9   | 20  |
+-----+-----+-----+
| 8   |FREE!| 16  |
+-----+-----+-----+
| 1   | 15  | 19  |
+-----+-----+-----+

```
At this point I have shown that the program can print the whole deck and everything is formatted right
```

Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
s
Enter a file name of where you want to save the file: firstSave.txt

```
At this point I have shown that the program can save the deck to a file 
```

Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
x

Main menu:
C) Create a new deck
X) Exit the program

Enter a Main command (C, X)
c
What should the card size be? i.e. [3-16]: 1
What should the card size be? i.e. [3-16]: 16
How many cards should there be in the deck? i.e. [1-8192]: 2
What should the max number be on the cards? i.e. [272-999]: 272

```
Here we see that when you exit out of the deck menu you can make a new deck
```

Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
p
What card?: 0

Card #0
   B     I     N     G     O     D     A     R     L     Y     Z     E     M     P     U     X  
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 6   | 32  | 44  | 65  | 83  | 98  | 106 | 125 | 152 | 158 | 171 | 196 | 212 | 224 | 241 | 273 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 4   | 21  | 43  | 59  | 77  | 92  | 118 | 120 | 150 | 161 | 177 | 193 | 211 | 229 | 249 | 264 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 14  | 24  | 50  | 67  | 70  | 86  | 113 | 131 | 146 | 165 | 183 | 202 | 207 | 237 | 247 | 271 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 16  | 20  | 37  | 68  | 84  | 88  | 110 | 123 | 140 | 164 | 172 | 205 | 208 | 222 | 244 | 263 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 17  | 23  | 52  | 61  | 86  | 96  | 115 | 129 | 148 | 157 | 180 | 197 | 215 | 228 | 240 | 258 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 15  | 29  | 38  | 52  | 81  | 99  | 109 | 128 | 139 | 166 | 187 | 190 | 219 | 235 | 250 | 257 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 12  | 34  | 42  | 53  | 78  | 101 | 103 | 127 | 145 | 170 | 175 | 204 | 205 | 226 | 253 | 266 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 7   | 19  | 49  | 55  | 71  | 95  | 107 | 133 | 147 | 163 | 179 | 195 | 222 | 225 | 242 | 259 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 5   | 26  | 41  | 57  | 75  | 97  | 104 | 136 | 141 | 162 | 174 | 189 | 209 | 223 | 245 | 272 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 11  | 35  | 45  | 69  | 72  | 91  | 105 | 121 | 144 | 160 | 185 | 188 | 220 | 232 | 256 | 265 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 10  | 18  | 40  | 60  | 85  | 102 | 116 | 126 | 137 | 156 | 181 | 194 | 217 | 239 | 239 | 261 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 18  | 30  | 35  | 66  | 69  | 87  | 114 | 134 | 138 | 168 | 184 | 200 | 216 | 238 | 251 | 256 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 8   | 33  | 47  | 58  | 73  | 89  | 119 | 130 | 149 | 155 | 182 | 203 | 213 | 231 | 254 | 267 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 3   | 25  | 46  | 62  | 82  | 103 | 112 | 135 | 153 | 169 | 186 | 199 | 210 | 234 | 243 | 270 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 1   | 22  | 36  | 63  | 74  | 94  | 111 | 132 | 142 | 171 | 178 | 201 | 218 | 227 | 248 | 268 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 2   | 27  | 39  | 54  | 76  | 90  | 108 | 124 | 143 | 159 | 176 | 198 | 214 | 230 | 246 | 262 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+


```
Notice that the even sized card doesn't have a FREE! cell in the middle while a odd card does.
```

Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
d
Card #0
   B     I     N     G     O     D     A     R     L     Y     Z     E     M     P     U     X  
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 6   | 32  | 44  | 65  | 83  | 98  | 106 | 125 | 152 | 158 | 171 | 196 | 212 | 224 | 241 | 273 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 4   | 21  | 43  | 59  | 77  | 92  | 118 | 120 | 150 | 161 | 177 | 193 | 211 | 229 | 249 | 264 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 14  | 24  | 50  | 67  | 70  | 86  | 113 | 131 | 146 | 165 | 183 | 202 | 207 | 237 | 247 | 271 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 16  | 20  | 37  | 68  | 84  | 88  | 110 | 123 | 140 | 164 | 172 | 205 | 208 | 222 | 244 | 263 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 17  | 23  | 52  | 61  | 86  | 96  | 115 | 129 | 148 | 157 | 180 | 197 | 215 | 228 | 240 | 258 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 15  | 29  | 38  | 52  | 81  | 99  | 109 | 128 | 139 | 166 | 187 | 190 | 219 | 235 | 250 | 257 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 12  | 34  | 42  | 53  | 78  | 101 | 103 | 127 | 145 | 170 | 175 | 204 | 205 | 226 | 253 | 266 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 7   | 19  | 49  | 55  | 71  | 95  | 107 | 133 | 147 | 163 | 179 | 195 | 222 | 225 | 242 | 259 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 5   | 26  | 41  | 57  | 75  | 97  | 104 | 136 | 141 | 162 | 174 | 189 | 209 | 223 | 245 | 272 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 11  | 35  | 45  | 69  | 72  | 91  | 105 | 121 | 144 | 160 | 185 | 188 | 220 | 232 | 256 | 265 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 10  | 18  | 40  | 60  | 85  | 102 | 116 | 126 | 137 | 156 | 181 | 194 | 217 | 239 | 239 | 261 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 18  | 30  | 35  | 66  | 69  | 87  | 114 | 134 | 138 | 168 | 184 | 200 | 216 | 238 | 251 | 256 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 8   | 33  | 47  | 58  | 73  | 89  | 119 | 130 | 149 | 155 | 182 | 203 | 213 | 231 | 254 | 267 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 3   | 25  | 46  | 62  | 82  | 103 | 112 | 135 | 153 | 169 | 186 | 199 | 210 | 234 | 243 | 270 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 1   | 22  | 36  | 63  | 74  | 94  | 111 | 132 | 142 | 171 | 178 | 201 | 218 | 227 | 248 | 268 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 2   | 27  | 39  | 54  | 76  | 90  | 108 | 124 | 143 | 159 | 176 | 198 | 214 | 230 | 246 | 262 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+

Card #1
   B     I     N     G     O     D     A     R     L     Y     Z     E     M     P     U     X  
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 12  | 28  | 48  | 53  | 74  | 95  | 111 | 133 | 137 | 159 | 177 | 197 | 214 | 230 | 251 | 270 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 17  | 29  | 38  | 66  | 82  | 92  | 114 | 121 | 142 | 170 | 172 | 196 | 212 | 239 | 247 | 264 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 6   | 34  | 36  | 69  | 79  | 97  | 115 | 137 | 153 | 168 | 175 | 193 | 207 | 231 | 256 | 271 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 8   | 31  | 52  | 56  | 70  | 94  | 109 | 126 | 147 | 166 | 181 | 198 | 211 | 237 | 248 | 256 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 5   | 33  | 50  | 54  | 78  | 101 | 107 | 135 | 138 | 154 | 184 | 205 | 220 | 236 | 254 | 261 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 7   | 22  | 40  | 52  | 71  | 100 | 106 | 127 | 151 | 163 | 186 | 192 | 208 | 226 | 252 | 258 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 14  | 19  | 39  | 57  | 75  | 91  | 117 | 134 | 144 | 158 | 187 | 194 | 215 | 224 | 245 | 257 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 10  | 20  | 45  | 62  | 81  | 96  | 116 | 136 | 149 | 167 | 171 | 203 | 206 | 234 | 242 | 259 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 11  | 32  | 47  | 65  | 80  | 89  | 108 | 131 | 140 | 162 | 178 | 195 | 205 | 233 | 239 | 273 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 4   | 35  | 44  | 64  | 86  | 93  | 120 | 128 | 143 | 165 | 174 | 189 | 216 | 225 | 244 | 266 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 15  | 30  | 41  | 58  | 76  | 99  | 110 | 130 | 152 | 169 | 180 | 204 | 221 | 232 | 243 | 260 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 16  | 27  | 46  | 68  | 83  | 86  | 118 | 122 | 154 | 160 | 182 | 200 | 213 | 229 | 246 | 262 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 13  | 18  | 37  | 63  | 85  | 103 | 113 | 132 | 139 | 161 | 179 | 202 | 210 | 228 | 249 | 268 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 18  | 25  | 49  | 55  | 69  | 90  | 112 | 123 | 141 | 171 | 173 | 190 | 209 | 235 | 250 | 269 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 3   | 24  | 42  | 67  | 84  | 88  | 103 | 125 | 146 | 155 | 183 | 188 | 222 | 227 | 253 | 263 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
| 9   | 21  | 43  | 60  | 72  | 87  | 119 | 124 | 145 | 157 | 176 | 201 | 217 | 223 | 255 | 265 |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+



Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
s
Enter a file name of where you want to save the file: firstSave.txt

Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
x

Main menu:
C) Create a new deck
X) Exit the program

Enter a Main command (C, X)
x
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn4 %  
```
Now I have shown that my program in the face of bad and good input still works. If a user wants to write to a file that doesn't exist the program will make a new file with the name entered. If the user want to save the file to a directory that doesn't exist then the program will crash per request of management.
```
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn4 % python3 src/bingo.py

 ########   ####  ##    ##   ######     #######   ####
 ##     ##   ##   ###   ##  ##    ##   ##     ##  ####
 ##     ##   ##   ####  ##  ##         ##     ##  ####
 ########    ##   ## ## ##  ##   ####  ##     ##   ##
 ##     ##   ##   ##  ####  ##    ##   ##     ##
 ##     ##   ##   ##   ###  ##    ##   ##     ##  ####
 ########   ####  ##    ##   ######     #######   ####

    Welcome to the DuckieCorp Bingo! Deck Generator

Main menu:
C) Create a new deck
X) Exit the program

Enter a Main command (C, X)
c
What should the card size be? i.e. [3-16]: 3
How many cards should there be in the deck? i.e. [1-8192]: 3
What should the max number be on the cards? i.e. [12-999]: 20

Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
s
Enter a file name of where you want to save the file: hello/save.txt
Traceback (most recent call last):
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn4/src/bingo.py", line 32, in <module>
    UserInterface().run()  	    	       
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn4/src/UserInterface.py", line 68, in run
    self.__create_deck()  	    	       
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn4/src/UserInterface.py", line 109, in __create_deck
    self.__deck_menu()
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn4/src/UserInterface.py", line 130, in __deck_menu
    self.__save_deck()  	    	       
  File "/Users/nathanstott/CS1440/cs1440-Stott-Nate-assn4/src/UserInterface.py", line 182, in __save_deck
    file = open(fileName, mode='w')
FileNotFoundError: [Errno 2] No such file or directory: 'hello/save.txt'
nathanstott@Nathans-MacBook-Air cs1440-Stott-Nate-assn4 % 
```

Additionally, my code passed all the unittests after writing the testCard.py file, and then I had to fix the testDeck.py to work with my code.


## Phase 5: Deployment *(5%)*

* Your repository pushed to GitLab.
  * Ok
* **Verify** that your final commit was received by browsing to its project page on GitLab.
    * Ensure the project's URL is correct.
      * Ok
    * Review the project to ensure that all required files are present and in correct locations.
      * Ok
    * Check that unwanted files have not been included.
      * Ok
    * Make any final touches to documentation, including the Sprint Signature and this Plan.
      * Ok
* **Validate** that your submission is complete and correct by cloning it to a new location on your computer and re-running it.
    * Run your program from the command line so you can see how it will behave when your grader runs it.  **Running it in PyCharm is not good enough!**
      * Ok
    * Run through your test cases to avoid nasty surprises.
      * Ok
    * Check that your documentation files are all present.
      * Ok


## Phase 6: Maintenance

* Write brief and honest answers to these questions: *(Note: do this before you complete **Phase 5: Deployment**)*
    * What parts of your program are sloppily written and hard to understand?
        * Are there parts of your program which you aren't quite sure how/why they work?
          * *No I get it all. But the RandNumberSet is pretty confusing.*
        * If a bug is reported in a few months, how long would it take you to find the cause?
          * *A day most likely*
    * Will your documentation make sense to...
        * ...anybody besides yourself?
          * *If you read the code and play around with the program I am sure anyone can understand the code*
        * ...yourself in six month's time?
          * *Yes the program is simple*
    * How easy will it be to add a new feature to this program in a year?
      * *Pretty easy the classes and different files break up the program into manageable sizes.*
    * Will your program continue to work after upgrading...
        * ...your computer's hardware?
          * *Yes all you need is the python interpreter*
        * ...the operating system?
          * *Yes all you need is the python interpreter*
        * ...to the next version of Python?
          * *I would assume so since the program uses basic python elements*
* Fill out the Assignment Reflection on Canvas.
  * *ok*
