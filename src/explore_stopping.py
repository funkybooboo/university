import random
import matplotlib.pyplot as plt
# TODO import numpy as np

# Part 1 - Brute force
# TODO make part 1 better

# Constants
number_of_candidates = 50
number_of_experiments = 1000

solution_found_count = {}
optimal_solution_found_count = {}

# This just creates a zero dictionary
for i in range(1, number_of_candidates + 1):
    solution_found_count[str(i)] = 0
    optimal_solution_found_count[str(i)] = 0

candidates = []
optimal_candidate = 0
for _ in range(number_of_experiments):
    candidates = random.sample(range(0, 1000), number_of_candidates)
    optimal_candidate = max(candidates)

    for i in range(1, number_of_candidates + 1):
        for candidate in candidates[i:-1]:
            if candidate > max(candidates[0:i]):
                solution_found_count[str(i)] += 1
                if candidate == optimal_candidate:
                    optimal_solution_found_count[str(i)] += 1
                break

print(candidates)
print(optimal_candidate)

x, y = zip(*optimal_solution_found_count.items())

print(x)
print(y)
print(optimal_solution_found_count)

plt.plot(x, y)
plt.show()

# Part 2 - Relax Distribution

# homepage.divms.uiowa.edu/~mbognar/applets/beta.html
# play around with the distribution

# Part 3 - Investment Decisions

# what happens to the optimal stop if you have to pay to check
