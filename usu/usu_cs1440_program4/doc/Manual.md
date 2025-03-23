# Bingo! User Manual  	         	  

## Running The Program
To run the program you can run this command in the terminal
```
python src/bingo.py
```
Note that *python* is the interpreter that we want our program to use and that *src/bingo.py* is a relative path to what directory you are currently in. 

After entering this command you will be presented with the Main menu. 


## Menu And Functionality

```

'########::'####:'##::: ##::'######::::'#######::'####:
 ##.... ##:. ##:: ###:: ##:'##... ##::'##.... ##: ####:
 ##:::: ##:: ##:: ####: ##: ##:::..::: ##:::: ##: ####:
 ########::: ##:: ## ## ##: ##::'####: ##:::: ##:: ##::
 ##.... ##:: ##:: ##. ####: ##::: ##:: ##:::: ##::..:::
 ##:::: ##:: ##:: ##:. ###: ##::: ##:: ##:::: ##:'####:
 ########::'####: ##::. ##:. ######:::. #######:: ####:
........:::....::..::::..:::......:::::.......:::....::

    Welcome to the DuckieCorp Bingo! Deck Generator

Main menu:
C - Create a new deck
X - Exit the program

Enter a Main command (C, X)
```

This is the programs Main menu and this is how you can start using the program or exit the program.

Note that you can use X or x to exit, and C or c to create a new deck. The menu doesn't care if a letter is upper or lower case. If you give invalid input then the menu will print out again.

If you choose to exit then the program will end with an exit code of 0 meaning that nothing went wrong while running the program.

If you choose to creat a new deck then you will be brought to a deck menu that will ask you some questions about your deck.

```       
    What should the card size be? i.e. [3-16]:                          
```
You can answer with only a whole number between 3 and 16 inclusive. If you give invalid input then the menu will print out again. Note if you pick an even card size then there won't be a FREE! space in the middle of the card since there is no middle.

```
    How many cards should there be in the deck? i.e. [1-8192]:
```
You can answer with only a whole number between 1 and 20 inclusive. If you give invalid input then the menu will print out again.

```
    What should the max number be on the cards? i.e. [27-35]: 
```
The numbers on this part will change depending on how big you made your card size be but in this example its 27-35 inclusive. If you give invalid input then the menu will print out again.

At this point the Deck menu will appear.
```
Deck menu:       
P - Print a card to the screen    	       
D - Display the whole deck to the screen    	       
S - Save the whole deck to a file 	       
X - Return to the Main menu

Enter a Main command (P, D, S, X)
```

Here you can select if you want to print a card to the screen by entering P or p.
```
Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
p
What card? [0-0]: 0

Card #0
   B     I     N  
+-----+-----+-----+
| 6   | 16  | 30  |
+-----+-----+-----+
| 3   |FREE!| 22  |
+-----+-----+-----+
| 4   | 14  | 26  |
+-----+-----+-----+

```
Notice that the only options for a card to be printed is 0 or 0 because we only have one card in the deck. The menu is then shown again, so you can make another option.


Or you can choose to display the whole deck to the screen by entering D or d. This will print the whole deck. The menu is then shown again, so you can make another option.

Or you can choose to save the deck to a file by entering S or s.
```
Deck menu:
P) Print a card to the screen
D) Display the whole deck to the screen
S) Save the whole deck to a file
X) Return to the Main menu

Enter a Deck command (P, D, S, X)
s
Enter a file name of where you want to save the file: SaveTheDeck.txt
```
This will make the deck be saved to a called SaveTheDeck.txt even if there isnt a file called SaveTheDeck.txt already the program will make one, and it will put it in your current directory. If there already is a file named SaveTheDeck.txt then the program will erase whatever is in there and save the deck. If you want to specify where you want the file to be saved then you need to have a valid file path, or you will see the program crash. 

Or you can return to the Main menu by entering X or x. This will take you back to the Main menu.


## Common Errors and How to Fix Them

A common error that users of the program see are the menu being printed out over and over again. That means that you are not putting in valid input and the program will just keep asking for valid input until it is given it or if the user closes the program. To fix this error make sure you are reading what the menu is asking for and give the program a valid option.

Another common error that users see a lot if when the program crashes with a scary message that might not make sense if you don't know what you are looking at. If that message comes up that means that you didn't put in a valid file path to save your deck and the program stopped at that point. To fix this make sure that you are giving the program a valid file path and you can get this by right-clicking on the file where you want to save the deck and then go to the option that says "copy absolute path" and click that then paste that path into the program when it asks you for a file path to save the deck.
