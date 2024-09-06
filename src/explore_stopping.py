import random
import matplotlib.pyplot as plt

# Part 1 - Brute force

NUMBER_OF_CANDIDATES = 50
NUMBER_OF_EXPERIMENTS = 1000

def main():
    # Initialize dictionaries with zero values
    position_was_optimal_count = {str(i): 0 for i in range(1, NUMBER_OF_CANDIDATES + 1)}
    optimal_solution_found_count = {str(i): 0 for i in range(1, NUMBER_OF_CANDIDATES + 1)}

    for experiment in range(NUMBER_OF_EXPERIMENTS):
        candidates = random.sample(range(1000), NUMBER_OF_CANDIDATES)
        optimal_candidate = max(candidates)
        print("---------")
        print(f"experiment {experiment}")
        print(f"optimal_candidate: {optimal_candidate}")
        print(f"candidates: {candidates}")
        print("---------")

        # skip the start because you're not going stop on the first person
        for i in range(1, NUMBER_OF_CANDIDATES + 1):
            # exclude the end person because that's not an optimal stopping (you went threw the whole thing)
            for candidate in candidates[i:-1]:
                # if they are better than what we have seen so far
                if candidate > max(candidates[0:i]):
                    # number of times that position was best threw all the experiments at any point in time
                    position_was_optimal_count[str(i)] += 1
                    if candidate == optimal_candidate:
                        # record that we have found /the optimal/ solution
                        optimal_solution_found_count[str(i)] += 1 / NUMBER_OF_EXPERIMENTS
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


if __name__ == "__main__":
    main()