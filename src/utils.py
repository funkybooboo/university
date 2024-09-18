import numpy as np
from typing import List, Callable


def get_average_rewards(
        algorithm: Callable[[], np.ndarray], num_runs: int
) -> np.ndarray:
    all_rewards: np.ndarray = np.zeros((num_runs, 10000))

    for run in range(num_runs):
        all_rewards[run] = algorithm()

    return np.mean(all_rewards, axis=0)


def get_choices(drift: float = 0) -> List[float]:
    base_choices: List[float] = [
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
    choices: List[float] = [p + drift for p in base_choices]
    return choices
