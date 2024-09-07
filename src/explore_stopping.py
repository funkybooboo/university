import random
import matplotlib.pyplot as plt

def main():
    run_and_plot_experiments_with_uniform_distribution()

# Part 1
def run_and_plot_experiments_with_uniform_distribution():
    NUMBER_OF_CANDIDATES = 50
    NUMBER_OF_EXPERIMENTS = 1000

    position_was_optimal_count = {str(i): 0 for i in range(1, NUMBER_OF_CANDIDATES + 1)}
    optimal_solution_found_count = {str(i): 0 for i in range(1, NUMBER_OF_CANDIDATES + 1)}

    for _ in range(NUMBER_OF_EXPERIMENTS):
        candidates = random.sample(range(1000), NUMBER_OF_CANDIDATES)
        run_experiment(candidates, optimal_solution_found_count, position_was_optimal_count, NUMBER_OF_CANDIDATES, NUMBER_OF_EXPERIMENTS)

    plot(position_was_optimal_count, optimal_solution_found_count)
# End Part 1

# Part 2
def run_and_plot_experiments_with_normal_distribution(alpha, beta):
    NUMBER_OF_CANDIDATES = 50
    NUMBER_OF_EXPERIMENTS = 1000

    # homepage.divms.uiowa.edu/~mbognar/applets/beta.html
    # play around with the distribution
    pass
# End Part 2

# Part 3
def run_and_plot_experiments_with_uniform_distribution_and_penalty(penalty):
    NUMBER_OF_CANDIDATES = 50
    NUMBER_OF_EXPERIMENTS = 1000

    pass

def run_and_plot_experiments_with_normal_distribution_and_penalty(alpha, beta, penalty):
    NUMBER_OF_CANDIDATES = 50
    NUMBER_OF_EXPERIMENTS = 1000

    # what happens to the optimal stop if you have to pay to check
    pass
# End Part 3

# Test
def run_and_plot_experiment_with_csv(csv_path):
    NUMBER_OF_CANDIDATES = 1000
    NUMBER_OF_EXPERIMENTS = 1
    position_was_optimal_count = {str(i): 0 for i in range(1, NUMBER_OF_CANDIDATES + 1)}
    optimal_solution_found_count = {str(i): 0 for i in range(1, NUMBER_OF_CANDIDATES + 1)}

    with open(csv_path) as csv_file:
        candidates = csv_file.readlines()
    run_experiment(candidates, optimal_solution_found_count, position_was_optimal_count, NUMBER_OF_CANDIDATES, NUMBER_OF_EXPERIMENTS)

    plot(position_was_optimal_count, optimal_solution_found_count)
# End Test

# Helper Functions
def run_experiment(candidates, optimal_solution_found_count, position_was_optimal_count, NUMBER_OF_CANDIDATES, NUMBER_OF_EXPERIMENTS):
    optimal_candidate = max(candidates)
    # skip the start because you're not going stop on the first person
    for i in range(1, NUMBER_OF_CANDIDATES + 1):
        # exclude the end person because that's not an optimal stopping (you went threw the whole thing)
        for candidate in candidates[i:-1]:
            # if they are better than what we have seen so far
            if candidate > max(candidates[0:i]):
                # number of times that position was best threw all the experiments at any point in time
                position_was_optimal_count[str(i)] += 1 / NUMBER_OF_EXPERIMENTS
                if candidate == optimal_candidate:
                    # record that we have found /the optimal/ solution
                    optimal_solution_found_count[str(i)] += 1 / NUMBER_OF_EXPERIMENTS
                break

def plot(position_was_optimal_count, optimal_solution_found_count):
    positions, times_optimal = zip(*optimal_solution_found_count.items())
    print("---------")
    print("final results")
    print(f"positions: {positions}")
    print(f"times_optimal: {times_optimal}")
    print("---------")

    plt.plot(positions, times_optimal, label="Optimal solution")
    plt.xlabel('Positions')
    plt.ylabel('Percent of optimal discovered')
    plt.grid(True)

    max_y = max(times_optimal)
    max_x = times_optimal.index(max_y)
    print(f"{max_y} --- {max_x} out of  {len(positions)}")

    max_y = max(times_optimal)
    max_y_x_position = times_optimal.index(max_y)

    plt.annotate(f'Max Optimal: ({max_y_x_position}, {max_y})',
                 xy=(max_y_x_position, max_y),
                 xytext=(max_y_x_position + 0.5, max_y),
                 arrowprops=dict(facecolor='black', arrowstyle='->'))

    plt.show()
# End Helper Functions

if __name__ == "__main__":
    main()
