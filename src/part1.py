import numpy as np
import matplotlib.pyplot as plt
from typing import List, Callable


num_steps: int = 10000
num_runs: int = 100
num_actions: int = 21


def main() -> None:
    plt.figure(figsize=(14, 8))

    epsilon_values: List[float] = [0.01, 0.05, 0.1, 0.4]
    for epsilon in epsilon_values:
        average_rewards: np.ndarray = get_average_rewards(
            lambda: epsilon_greedy_algorithm(epsilon)
        )
        plt.plot(average_rewards, label=f'Epsilon-Greedy, Epsilon = {epsilon}')

    average_rewards: np.ndarray = get_average_rewards(
        lambda: thompson_sampling_algorithm()
    )
    plt.plot(average_rewards, label='Thompson Sampling', linestyle='--')

    plt.xlabel('Steps')
    plt.ylabel('Average Reward')
    plt.title('Best Convergence Results of Epsilon-Greedy and Thompson Sampling')
    plt.legend()
    plt.grid(True)
    plt.show()


def get_average_rewards(
        algorithm: Callable[[], np.ndarray]
) -> np.ndarray:
    all_rewards = np.zeros((num_runs, num_steps))

    for run in range(num_runs):
        rewards = algorithm()
        all_rewards[run] = rewards

    average_rewards = np.mean(all_rewards, axis=0)

    return average_rewards


def get_probabilities(drift: float = 0) -> List[float]:
    base_probs: List[float] = [
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
        np.random.normal(-4.5, 8)
    ]
    probs: List[float] = [p + drift for p in base_probs]
    return probs


def epsilon_greedy_algorithm(epsilon: float) -> np.ndarray:
    action_averages: np.ndarray = np.array(get_probabilities())
    action_count: np.ndarray = np.ones(num_actions)
    rewards: List[float] = []

    for step in range(num_steps):
        action: int = int(
            np.random.randint(num_actions)
            if np.random.rand() < epsilon
            else np.argmax(action_averages)
        )
        probabilities: List[float] = get_probabilities()
        reward: float = np.random.normal(probabilities[action], 1)

        action_count[action] += 1
        action_averages[action] += (reward - action_averages[action]) / action_count[action]
        rewards.append(reward)

    return np.cumsum(rewards) / (np.arange(num_steps) + 1)


def thompson_sampling_algorithm() -> np.ndarray:
    # Initialize success and failure counts for each arm
    successes = np.ones(num_actions)  # Alpha in Beta distribution
    failures = np.ones(num_actions)   # Beta in Beta distribution
    rewards: List[float] = []

    for step in range(num_steps):
        # Sample from the Beta distribution for each arm
        sampled_values = np.random.beta(successes, failures)

        # Select the action with the highest sampled value
        action = int(np.argmax(sampled_values))

        # Simulate reward from the probability distribution
        probabilities = get_probabilities()  # Get the true probabilities
        reward = np.random.normal(probabilities[action], 1)  # Use normal distribution

        # Update the counts based on the reward received
        if reward > 0:  # Assume reward is non-negative
            successes[action] += 1
        else:
            failures[action] += 1

        # Store the reward
        rewards.append(reward)

    return np.cumsum(rewards) / (np.arange(num_steps) + 1)


if __name__ == "__main__":
    main()
