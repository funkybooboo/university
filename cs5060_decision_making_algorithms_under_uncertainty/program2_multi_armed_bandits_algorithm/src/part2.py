import numpy as np
import matplotlib.pyplot as plt
from typing import List, Callable
from part1 import get_average_rewards, get_probabilities

# Constants for simulation
num_steps: int = 10000  # Total number of steps for each run
num_runs: int = 100     # Number of runs for averaging results
num_actions: int = 21   # Number of possible actions (bandits)

def linear_quench(t: int, total_steps: int) -> float:
    return max(0, 1 - t / total_steps)

def asymptotic_quench(t: int, total_steps: int) -> float:
    return 1 / (1 + 0.01 * t)

def heavy_asymptotic_quench(t: int, total_steps: int) -> float:
    return 1 / (1 + 0.0001 * t**2)

def epsilon_greedy_with_quenching(quench_func: Callable[[int, int], float]) -> np.ndarray:
    action_averages: np.ndarray = np.array(get_probabilities())  # Initialize action averages
    action_count: np.ndarray = np.ones(num_actions)  # Initialize action counts
    rewards: List[float] = []  # Store rewards for each step

    for step in range(num_steps):
        # Calculate epsilon using the quenching function
        epsilon = quench_func(step, num_steps)
        
        # Choose an action based on epsilon-greedy strategy with quenching
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

def p2p1() -> None:
    plt.figure(figsize=(14, 8))

    # Different quenching functions
    quenching_functions = [
        ('Linear Quenching', linear_quench),
        ('Asymptotic Quenching', asymptotic_quench),
        ('Heavy Asymptotic Quenching', heavy_asymptotic_quench)
    ]
    
    for label, quench_func in quenching_functions:
        average_rewards: np.ndarray = get_average_rewards(
            lambda: epsilon_greedy_with_quenching(quench_func)
        )
        plt.plot(average_rewards, label=label)
        print(label + ' done')
    
    # Plot configuration
    plt.xlabel('Steps')
    plt.ylabel('Average Reward')
    plt.title('Epsilon-Greedy with Quenching Strategies')
    plt.legend()
    plt.grid(True)
    plt.show()

def epsilon_greedy_exclude_best(choices: np.ndarray, epsilon: float, current_best: int) -> int:
    if np.random.random() < epsilon:
        return np.random.choice([i for i in range(len(choices)) if i != current_best])
    else:
        return current_best

def weighted_exploration(choices: np.ndarray, epsilon: float, counts: np.ndarray) -> int:
    if np.random.random() < epsilon:
        unexplored_probs = 1 / (counts + 1)  # Higher probability for less explored choices
        return np.random.choice(len(choices), p=unexplored_probs / unexplored_probs.sum())
    else:
        return np.argmax(choices)
    
def epsilon_greedy_with_exploration_strategy(strategy_func: Callable) -> np.ndarray:
    action_averages: np.ndarray = np.array(get_probabilities())  # Initialize action averages
    action_count: np.ndarray = np.ones(num_actions)  # Initialize action counts
    rewards: List[float] = []  # Store rewards for each step

    for step in range(num_steps):
        # Determine the current best action
        current_best: int = np.argmax(action_averages)

        # Choose an action based on the strategy function
        if strategy_func == epsilon_greedy_exclude_best:
            action: int = strategy_func(action_averages, epsilon=0.1, current_best=current_best)
        else:
            action: int = strategy_func(action_averages, epsilon=0.1, counts=action_count)
        
        probabilities: List[float] = get_probabilities()  # Get current probabilities
        reward: float = np.random.normal(probabilities[action], 1)  # Simulate reward

        # Update action counts and averages
        action_count[action] += 1
        action_averages[action] += (reward - action_averages[action]) / action_count[action]
        rewards.append(reward)  # Store the received reward

    return np.cumsum(rewards) / (np.arange(num_steps) + 1)  # Return cumulative average rewards

def p2p2() -> None:
    plt.figure(figsize=(14, 8))

    # Different exploration strategies
    exploration_strategies = [
        ('Exclude Best', epsilon_greedy_exclude_best),
        ('Weighted Exploration', weighted_exploration)
    ]
    
    for label, strategy_func in exploration_strategies:
        average_rewards: np.ndarray = get_average_rewards(
            lambda: epsilon_greedy_with_exploration_strategy(strategy_func)
        )
        plt.plot(average_rewards, label=label)
        print(label + ' done')
    # Plot configuration
    plt.xlabel('Steps')
    plt.ylabel('Average Reward')
    plt.title('Epsilon-Greedy with Modified Exploration Strategies')
    plt.legend()
    plt.grid(True)
    plt.show()
if __name__ == "__main__":
    p2p1()
    p2p2()