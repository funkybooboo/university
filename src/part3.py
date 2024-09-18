import numpy as np
import matplotlib.pyplot as plt
from typing import Callable, List

from utils import get_probabilities


def main() -> None:
    num_steps: int = 10000
    num_runs: int = num_steps // 100

    plt.figure(figsize=(14, 8))

    # Testing Epsilon-Greedy in a dynamic environment
    epsilon_values: List[float] = [0.01, 0.05, 0.1, 0.4]
    for epsilon in epsilon_values:
        best_rewards = get_best_rewards(
            lambda: dynamic_epsilon_greedy_algorithm(epsilon, num_steps=num_steps),
            num_runs,
        )
        plt.plot(best_rewards, label=f'Dynamic Epsilon-Greedy, Epsilon = {epsilon}')

    # Testing Thompson Sampling in a dynamic environment
    best_rewards_thompson = get_best_rewards(
        lambda: dynamic_thompson_sampling_algorithm(num_steps=num_steps), num_runs
    )
    plt.plot(best_rewards_thompson, label='Dynamic Thompson Sampling', linestyle='--')

    # Testing Restarted Thompson Sampling
    best_rewards_restarted = get_best_rewards(
        lambda: restarted_thompson_sampling_algorithm(num_steps=num_steps), num_runs
    )
    plt.plot(best_rewards_restarted, label='Restarted Thompson Sampling', linestyle=':')

    plt.xlabel('Steps')
    plt.ylabel('Average Reward')
    plt.title('Dynamic Environment: Epsilon-Greedy vs. Thompson Sampling')
    plt.legend()
    plt.grid(True)
    plt.show()


def get_best_rewards(algorithm: Callable[[], np.ndarray], num_runs: int) -> np.ndarray:
    best_convergence: float = -np.inf
    best_rewards: np.ndarray = np.empty((0,))

    for _ in range(num_runs):
        rewards: np.ndarray = algorithm()
        avg_last_segment: float = float(
            np.mean(rewards[-100:])
        )  # Average of last 100 steps

        if avg_last_segment > best_convergence:
            best_convergence = avg_last_segment
            best_rewards = rewards
    return best_rewards


def drifting_probabilities(
    num_actions: int, drift: float = -0.001, step: int = 0
) -> List[float]:
    base_probs = get_probabilities()
    for action in range(num_actions):
        base_probs[action] += drift * step
    return base_probs


def apply_sudden_shifts(probs: List[float], step: int) -> List[float]:
    if step == 3000:
        probs[0] += 7
        probs[2] += 3
        probs[7] += 1
        probs[18] += 2

        # If more than 3 standard deviations away for Bandit 7
        if np.abs(probs[7]) > 3 * 7:  # Assuming std. dev. is 7
            probs[7] = 50
    return probs


def run_dynamic_algorithm(
    num_steps: int, num_actions: int, restart: bool = False
) -> np.ndarray:
    mu = np.zeros(num_actions)
    sigma2 = np.ones(num_actions) * 1e-6
    rewards = []

    for step in range(num_steps):
        probs = drifting_probabilities(num_actions, step=step)
        probs = apply_sudden_shifts(probs, step)

        if restart and step == 3000:
            mu.fill(0)
            sigma2.fill(1e-6)  # Restart parameters

        sampled_means = np.random.normal(mu, np.sqrt(sigma2))
        action = int(np.argmax(sampled_means))
        reward = np.random.normal(probs[action], 1)

        mu[action] = (mu[action] * sigma2[action] + reward) / (sigma2[action] + 1)
        sigma2[action] = 1 / (1 / sigma2[action] + 1)
        rewards.append(reward)

    return np.cumsum(rewards) / (np.arange(num_steps) + 1)


def dynamic_epsilon_greedy_algorithm(
    epsilon: float, num_steps: int = 10000, num_actions: int = 20
) -> np.ndarray:
    Q = np.zeros(num_actions)
    N = np.zeros(num_actions)
    rewards = []

    for step in range(num_steps):
        probs = drifting_probabilities(num_actions, step=step)
        probs = apply_sudden_shifts(probs, step)

        action = int(
            np.random.randint(num_actions)
            if np.random.rand() < epsilon
            else np.argmax(Q)
        )
        reward = np.random.normal(probs[action], 1)

        N[action] += 1
        Q[action] += (reward - Q[action]) / N[action]
        rewards.append(reward)

    return np.cumsum(rewards) / (np.arange(num_steps) + 1)


def dynamic_thompson_sampling_algorithm(
    num_steps: int = 10000, num_actions: int = 20
) -> np.ndarray:
    return run_dynamic_algorithm(num_steps, num_actions)


def restarted_thompson_sampling_algorithm(
    num_steps: int = 10000, num_actions: int = 20
) -> np.ndarray:
    return run_dynamic_algorithm(num_steps, num_actions, restart=True)


if __name__ == "__main__":
    main()
