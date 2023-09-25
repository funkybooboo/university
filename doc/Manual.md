# Recursive Web Crawler User Manual

### How to run:

Make sure that you are in the head of the project in order to run the project.

Here is an example of valid input:

```
% python3 src/crawler.py https://cs.usu.edu 1
```

Let's break down what is happening

1. "python3" This is the python interpreter that will read the code.
2. "src/crawler.py" This is the file that we want to execute.
3. "https://cs.usu.edu" This is the url that we want the program to use as the starting url.
4. "1" This is an optional parameter. It specifies how many pages deep do we want the program to search. If left blank then the program will go 3 pages deep.

The program is pretty simple and will tell you want to do in the case of failure.

Here is the output of the example input:
```
Crawling from https://cs.usu.edu to a maximum depth of 1 link
    https://cs.usu.edu
Program took 0.45 seconds to run. Number of unique urls visited: 1
```
