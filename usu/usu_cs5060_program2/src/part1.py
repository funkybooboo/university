import numpy as np
import matplotlib.pyplot as plt
from typing import List, Callable

# Constants for simulation
num_steps: int = 10000  # Total number of steps for each run
num_runs: int = 100     # Number of runs for averaging results
num_actions: int = 21   # Number of possible actions (bandits)

def main() -> None:
    plt.figure(figsize=(14, 8))

    # Different epsilon values for the Epsilon-Greedy algorithm
    epsilon_values: List[float] = [0.01, 0.05, 0.1, 0.4]
    for epsilon in epsilon_values:
        average_rewards: np.ndarray = get_average_rewards(
            lambda: epsilon_greedy_algorithm(epsilon)
        )
        plt.plot(average_rewards, label=f'Epsilon-Greedy, Epsilon = {epsilon}')

    # Run Thompson Sampling and plot its results
    average_rewards: np.ndarray = get_average_rewards(
        lambda: thompson_sampling_algorithm()
    )
    plt.plot(average_rewards, label='Thompson Sampling', linestyle='--')

    # Plot configuration
    plt.xlabel('Steps')
    plt.ylabel('Average Reward')
    plt.title('Best Convergence Results of Epsilon-Greedy and Thompson Sampling')
    plt.legend()
    plt.grid(True)
    plt.show()

def get_average_rewards(algorithm: Callable[[], np.ndarray]) -> np.ndarray:
    all_rewards: np.ndarray = np.zeros((num_runs, num_steps))  # Store rewards for all runs

    for run in range(num_runs):
        rewards: np.ndarray = algorithm()  # Run the algorithm to get rewards
        all_rewards[run] = rewards  # Store the rewards for this run

    # Calculate the average rewards over all runs
    average_rewards: np.ndarray = np.mean(all_rewards, axis=0)
    return average_rewards

def get_probabilities() -> List[float]:
    probs: List[float] = [
        np.random.normal(0, 5),
        np.random.normal(-0.5, 12),
        np.random.normal(2, 3.9),
        np.random.normal(-0.5, 7),
        np.random.normal(-1.2, 8),
        np.random.normal(-3, 7),
        np.random.normal(-10, 20),
        np.random.normal(-0.5, 1),
        np.random.normal(-1, 2),
        np.random.normal(1, 6),
        np.random.normal(0.7, 4),
        np.random.normal(-6, 11),
        np.random.normal(-7, 1),
        np.random.normal(-0.5, 2),
        np.random.normal(-6.5, 1),
        np.random.normal(-3, 6),
        np.random.normal(0, 8),
        np.random.normal(2, 3.9),
        np.random.normal(-9, 12),
        np.random.normal(-1, 6),
        np.random.normal(-4.5, 8),
    ]
    return probs

def epsilon_greedy_algorithm(epsilon: float) -> np.ndarray:
    action_averages: np.ndarray = np.array(get_probabilities())  # Initialize action averages
    action_count: np.ndarray = np.ones(num_actions)  # Initialize action counts
    rewards: List[float] = []  # Store rewards for each step

    for step in range(num_steps):
        # Choose an action based on epsilon-greedy strategy
        action: int = int(
            np.random.randint(num_actions) if np.random.rand() < epsilon
            else np.argmax(action_averages)
        )
        probabilities: List[float] = get_probabilities()  # Get current probabilities
        reward: float = np.random.normal(probabilities[action], 1)  # Simulate reward

        # Update action counts and averages
        action_count[action] += 1
        action_averages[action] += (reward - action_averages[action]) / action_count[action]
        rewards.append(reward)  # Store the received reward

    return np.cumsum(rewards) / (np.arange(num_steps) + 1)  # Return cumulative average rewards

def thompson_sampling_algorithm() -> np.ndarray:
    # Initialize success and failure counts for each action
    successes: np.ndarray = np.ones(num_actions)  # Alpha in Beta distribution
    failures: np.ndarray = np.ones(num_actions)  # Beta in Beta distribution
    rewards: List[float] = []  # Store rewards for each step

    for step in range(num_steps):
        # Sample from the Beta distribution for each action
        sampled_values: np.ndarray = np.random.beta(successes, failures)
        # Select the action with the highest sampled value
        action: int = int(np.argmax(sampled_values))

        # Get current probabilities and simulate reward
        probabilities: List[float] = get_probabilities()  # Get the true probabilities
        reward: float = np.random.normal(probabilities[action], 1)  # Use normal distribution

        # Update success and failure counts based on the received reward
        if reward > 0:  # Assume reward is non-negative
            successes[action] += 1
        else:
            failures[action] += 1

        rewards.append(reward)  # Store the received reward

    return np.cumsum(rewards) / (np.arange(num_steps) + 1)  # Return cumulative average rewards

if __name__ == "__main__":
    main()  # Execute the main function
