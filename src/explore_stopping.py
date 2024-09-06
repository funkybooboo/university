import random
import matplotlib.pyplot as plt

# Part 1 - Brute force

number_of_candidates = 50
number_of_experiments = 1000

position_was_optimal_count = {}
optimal_solution_found_count = {}

# This just creates a zero dictionary
for i in range(1, number_of_candidates + 1):
    position_was_optimal_count[str(i)] = 0
    optimal_solution_found_count[str(i)] = 0

for experiment in range(number_of_experiments):
    candidates = random.sample(range(1000), number_of_candidates)
    optimal_candidate = max(candidates)
    print("---------")
    print(f"experiment {experiment}")
    print(f"optimal_candidate: {optimal_candidate}")
    print(f"candidates: {candidates}")
    print("---------")

    # skip the start because you're not going stop on the first person
    for i in range(1, number_of_candidates + 1):
        # exclude the end person because that's not an optimal stopping (you went threw the whole thing)
        for candidate in candidates[i:-1]:
            # if they are better than what we have seen so far
            if candidate > max(candidates[0:i]):
                # number of times that position was best threw all the experiments at any point in time
                position_was_optimal_count[str(i)] += 1
                if candidate == optimal_candidate:
                    # record that we have found /the optimal/ solution
                    optimal_solution_found_count[str(i)] += 1 / number_of_experiments
                break

positions, times_optimal = zip(*optimal_solution_found_count.items())

print("---------")
print("final results")
print(f"positions: {positions}")
print(f"times_optimal: {times_optimal}")
print("---------")

plt.plot(positions, times_optimal)
plt.show()

# Part 2 - Relax Distribution

# homepage.divms.uiowa.edu/~mbognar/applets/beta.html
# play around with the distribution

# Part 3 - Investment Decisions

# what happens to the optimal stop if you have to pay to check
