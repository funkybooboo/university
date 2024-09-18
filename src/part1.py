import numpy as np
import matplotlib.pyplot as plt
from typing import Callable, List

from utils import get_probabilities


def main() -> None:
    num_steps: int = 10000
    convergence_window: int = num_steps // 10
    num_runs: int = num_steps // 100

    plt.figure(figsize=(14, 8))

    epsilon_values: List[float] = [0.01, 0.05, 0.1, 0.4]
    for epsilon in epsilon_values:
        best_rewards: np.ndarray = get_best_rewards(
            lambda: epsilon_greedy_algorithm(epsilon, num_steps=num_steps),
            num_runs,
            convergence_window,
        )
        plt.plot(best_rewards, label=f'Epsilon-Greedy, Epsilon = {epsilon}')
        mark_convergence(best_rewards)

    best_rewards = get_best_rewards(
        lambda: thompson_sampling_algorithm(num_steps=num_steps),
        num_runs,
        convergence_window,
    )
    plt.plot(best_rewards, label='Thompson Sampling', linestyle='--')
    mark_convergence(best_rewards)

    plt.xlabel('Steps')
    plt.ylabel('Average Reward')
    plt.title('Best Convergence Results of Epsilon-Greedy and Thompson Sampling')
    plt.legend()
    plt.grid(True)
    plt.show()


def get_best_rewards(
    algorithm: Callable[[], np.ndarray], num_runs: int, convergence_window: int
) -> np.ndarray:
    best_convergence: float = -np.inf
    best_rewards: np.ndarray = np.empty((0,))

    for _ in range(num_runs):
        rewards: np.ndarray = algorithm()
        if len(rewards) < convergence_window:
            continue
        avg_last_segment: float = float(np.mean(rewards[-convergence_window:]))

        # Store the rewards if they show the best convergence
        if avg_last_segment > best_convergence:
            best_convergence = avg_last_segment
            best_rewards = rewards
    return best_rewards


def mark_convergence(rewards: np.ndarray) -> None:
    recent_avg: float = float(np.mean(rewards[-100:]))

    for step in range(len(rewards) - 1):
        if np.abs(rewards[step] - recent_avg) < 0.01:
            plt.annotate(
                'Converged',
                xy=(float(step), float(rewards[step])),
                xytext=(float(step + 100), float(rewards[step] + 0.1)),
                arrowprops=dict(facecolor='black', arrowstyle='->'),
                fontsize=10,
                color='black',
            )
            break


def epsilon_greedy_algorithm(
    epsilon: float, num_steps: int = 10000, num_actions: int = 20
) -> np.ndarray:
    Q: np.ndarray = np.zeros(num_actions)
    N: np.ndarray = np.zeros(num_actions)
    true_rewards: List[float] = get_probabilities()
    rewards: List[float] = []

    for step in range(num_steps):
        action: int = int(
            np.random.randint(num_actions)
            if np.random.rand() < epsilon
            else np.argmax(Q)
        )
        reward: float = np.random.normal(true_rewards[action], 1)

        N[action] += 1
        Q[action] += (reward - Q[action]) / N[
            action
        ]  # Update estimated value for action
        rewards.append(reward)

    return np.cumsum(rewards) / (np.arange(num_steps) + 1)


def thompson_sampling_algorithm(
    num_steps: int = 10000, num_actions: int = 20
) -> np.ndarray:
    mu: np.ndarray = np.zeros(num_actions)
    sigma2: np.ndarray = np.ones(num_actions) * 1e-6
    true_rewards: List[float] = get_probabilities()
    rewards: List[float] = []

    for step in range(num_steps):
        sampled_means: np.ndarray = np.random.normal(mu, np.sqrt(sigma2))
        action: int = int(np.argmax(sampled_means))
        reward: float = np.random.normal(true_rewards[action], 1)

        # Update posterior distribution parameters based on observed reward
        mu[action] = (mu[action] * sigma2[action] + reward) / (sigma2[action] + 1)
        sigma2[action] = 1 / (1 / sigma2[action] + 1)
        rewards.append(reward)

    return np.cumsum(rewards) / (np.arange(num_steps) + 1)


if __name__ == "__main__":
    main()
