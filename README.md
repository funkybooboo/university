# CS5060_Program1

## Team Members
- Ann Marie Humble
- Brighton Ellis
- Nate Stott
- Madison Patch

## Optimal Stopping Results
### Part One
Our algorithm (meaning Dr. Harper's algorithm, thank you Dr. Harper) 
consistently found the most optimal candidates 
after searching around 37% of the candidate pool. The best strategy 
for choosing a candidate, then, would be to interview and dismiss
the first 37% of candidates, and then accept the next best candidate after that.
However, this strategy still only finds the optimal candidate about
37% percent of the time. Which isn't _comforting_. But it's the best we can do.

A side note, it's kind of cool that, with this brute-force solution, we get 37% for the
likelihood of finding the optimal candidate, as for the amount of candidates 
to go through before making a choice.

### Part Two

### Part Three

## Instructions to Run

1. install [poetry](https://python-poetry.org/docs/) on your machine
    1. prob just run this `pipx install poetry`
2. navigate to our project in the terminal
3. `poetry shell`
4. `poetry install`
5. `python3 explore_stopping.py`
