import numpy as np
import matplotlib.pyplot as plt
from typing import List

from utils import get_choices, get_average_rewards


def main() -> None:
    num_actions: int = 10000
    num_runs: int = num_actions // 100

    plt.figure(figsize=(14, 8))

    epsilon_values: List[float] = [0.01, 0.05, 0.1, 0.4]
    for epsilon in epsilon_values:
        average_rewards: np.ndarray = get_average_rewards(
            lambda: epsilon_greedy_algorithm(epsilon, num_actions), num_runs
        )
        plt.plot(average_rewards, label=f'Epsilon-Greedy, Epsilon = {epsilon}')

    average_rewards = get_average_rewards(
        lambda: thompson_sampling_algorithm(num_actions), num_runs
    )
    plt.plot(average_rewards, label='Thompson Sampling', linestyle='--')

    plt.xlabel('Steps')
    plt.ylabel('Average Reward')
    plt.title('Best Convergence Results of Epsilon-Greedy and Thompson Sampling')
    plt.legend()
    plt.grid(True)
    plt.show()


def epsilon_greedy_algorithm(epsilon: float, num_actions: int) -> np.ndarray:
    pass


def thompson_sampling_algorithm(num_actions: int) -> np.ndarray:
    pass


if __name__ == "__main__":
    main()
