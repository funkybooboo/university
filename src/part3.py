import numpy as np
import matplotlib.pyplot as plt
from typing import List

from utils import get_choices, get_average_rewards


def main() -> None:
    num_actions: int = 10000
    num_runs: int = num_actions // 100

    plt.figure(figsize=(14, 8))

    # Testing Epsilon-Greedy in a dynamic environment
    epsilon_values: List[float] = [0.01, 0.05, 0.1, 0.4]
    for epsilon in epsilon_values:
        best_rewards = get_average_rewards(
            lambda: dynamic_epsilon_greedy_algorithm(epsilon, num_actions), num_runs
        )
        plt.plot(best_rewards, label=f'Dynamic Epsilon-Greedy, Epsilon = {epsilon}')

    # Testing Thompson Sampling in a dynamic environment
    best_rewards_thompson = get_average_rewards(
        lambda: dynamic_thompson_sampling_algorithm(num_actions), num_runs
    )
    plt.plot(best_rewards_thompson, label='Dynamic Thompson Sampling', linestyle='--')

    # Testing Restarted Thompson Sampling
    best_rewards_restarted = get_average_rewards(
        lambda: restarted_thompson_sampling_algorithm(num_actions), num_runs
    )
    plt.plot(best_rewards_restarted, label='Restarted Thompson Sampling', linestyle=':')

    plt.xlabel('Steps')
    plt.ylabel('Average Reward')
    plt.title('Dynamic Environment: Epsilon-Greedy vs. Thompson Sampling')
    plt.legend()
    plt.grid(True)
    plt.show()


def drift_choices(
    num_actions: int, drift: float = -0.001, step: int = 0
) -> List[float]:
    choices = get_choices()
    for action in range(num_actions):
        choices[action] += drift * step
    return choices


def apply_sudden_shifts(choices: List[float], step: int) -> List[float]:
    if step == 3000:
        choices[0] += 7
        choices[2] += 3
        choices[7] += 1
        choices[18] += 2

        # If more than 3 standard deviations away for Bandit 7
        if np.abs(choices[7]) > 3 * 7:  # Assuming std. dev. is 7
            choices[7] = 50
    return choices


def dynamic_epsilon_greedy_algorithm(
    epsilon: float, num_actions: int
) -> np.ndarray:
    pass


def dynamic_thompson_sampling_algorithm(
    num_actions: int
) -> np.ndarray:
    return run_dynamic_algorithm(num_actions)


def restarted_thompson_sampling_algorithm(
    num_actions: int
) -> np.ndarray:
    return run_dynamic_algorithm(num_actions, restart=True)


def run_dynamic_algorithm(
    num_actions: int, restart: bool = False
) -> np.ndarray:
    pass


if __name__ == "__main__":
    main()
