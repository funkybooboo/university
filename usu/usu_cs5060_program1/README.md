# CS5060_Program1

## Team Members
- Ann Marie Humble
- Brighton Ellis
- Nate Stott
- Madison Patch

## Optimal Stopping Results
Coordinates represent the following:
x = the optimal stopping point (by number of candidates)
y = the decimal percent of the likelihood of finding the best candidate

| vanilla Uniform | Normal | Skewed Beta | Uniform (Penalty) | Normal (Penalty) |
|--------------|--------------|--------------|-------------|-------------|
| ( 20, 0.37)  | ( 16, 0.41)  | ( 17, 0.34)  | ( 9, 0.34)  | ( 3, 0.40)  |
| ( 17, 0.39)  | ( 20, 0.37)  | ( 21, 0.38)  | ( 8, 0.32)  | ( 3, 0.35)  |
| ( 20, 0.38)  | ( 16, 0.42)  | ( 22, 0.38)  | ( 10, 0.31) | ( 2, 0.38)  |
| ( 21, 0.39)  | ( 19, 0.37)  | ( 15, 0.38)  | ( 8, 0.33)  | ( 3, 0.38)  |
| ( 16, 0.37)  | ( 17, 0.40)  | ( 18, 0.41)  | ( 10, 0.35) | ( 3, 0.37)  |
| ( 19, 0.39)  | ( 18, 0.36)  | ( 23, 0.38)  | ( 12, 0.33) | ( 2, 0.38)  |
| ( 19, 0.41)  | ( 22, 0.40)  | ( 18, 0.39)  | ( 9, 0.34)  | ( 2, 0.39)  |
| ( 22, 0.40)  | ( 16, 0.37)  | ( 16, 0.38)  | ( 11, 0.34) | ( 2, 0.36)  |
| ( 18, 0.39)  | ( 22, 0.39)  | ( 17, 0.38)  | ( 10, 0.34) | ( 2, 0.40)  |
| ( 19, 0.37)  | ( 19, 0.36)  | ( 17, 0.37)  | ( 9, 0.31)  | ( 2, 0.39)  |


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

The optimal stopping point in a group of candidates with scores following a 
skewed beta distribution and in a group of candidates with scores following a normal distribution is very similar. 
The Beta(2,7) skewed distribution had a 35-41% chance of finding the optimal candidate after 
typically between 15-22 candidate evaluations. The normal distribution had a 37-41% chance of finding 
the optimal candidate after typically between 15-23 candidate evaluations. 
This demonstrates that the distribution shape does not strongly influence the optimal stopping point as long 
as there are roughly the same number of candidates between distributions that share a score.


### Part Three

Using penalties on a **uniform distribution**, the optimal stopping position is shifted to the left by 30% (from position 20 to position 13), but still lies on or around 37%. Something we found interesting about this was that if you pick a candidate using this strategy, your chance of finding the optimal candidate goes down a little bit (roughly 5%), but you cut the time it takes to find a candidate in half. If you're short on time, this strategy looks pretty good.

Using penalties on a **normal distribution**, 
our graph showed that the optimal time to stop interviewing and start choosing was after candidate two. 
This has around a 37% chance of success (assuming sample size of 50 candidates). 
We were initially very surprised by these results, but they made sense after we thought about it. 
In a normal distribution, the vast majority of candidates will have the same (or roughly the same) qualifications. 
With a penalty being administered after each additional interview, 
you very quickly reach a point where the chance of finding an exceptionally good candidate is outweighed by the time (penalty) it takes to find that candidate. 
The best strategy is to get a rough benchmark of the candidates and then pick quickly. 
What was shocking to us was the extent to which the optimal stop was shifted to the left. 

## Instructions to Run

- Dependencies
  - matplotlib
  - numpy
- Run
  - navigate to project
  - python3 file-name.py
