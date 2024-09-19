import numpy as np
import matplotlib.pyplot as plt
from typing import List, Callable


num_steps: int = 10000
num_runs: int = 5


def main() -> None:
    plt.figure(figsize=(14, 8))

    epsilon_values: List[float] = [0.01, 0.05, 0.1, 0.4]
    for epsilon in epsilon_values:
        average_rewards: np.ndarray = get_average_rewards(
            lambda: epsilon_greedy_algorithm(epsilon)
        )
        plt.plot(average_rewards, label=f'Epsilon-Greedy, Epsilon = {epsilon}')

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
    rewards = []
    action_reward_histories = []

    # Do every action once
    for reward in get_probabilities():
        rewards.append(reward)
        action_reward_histories.append([reward])

    for step in range(num_steps - len(rewards)):
        probabilities = get_probabilities()
        if np.random.rand() < epsilon:
            # Exploit
            best_average_reward = float('-inf')
            reward_index = 0
            for action_index in range(len(action_reward_histories)):
                action_reward_history = action_reward_histories[action_index]
                average_reward = sum(action_reward_history) / len(action_reward_history)
                if average_reward > best_average_reward:
                    best_average_reward = average_reward
                    reward_index = action_index
        else:
            # Explore
            reward_index = np.random.choice(len(probabilities))
        reward = probabilities[reward_index]
        action_reward_histories[reward_index].append(reward)
        rewards.append(reward)

    return np.array(rewards)


if __name__ == "__main__":
    main()
