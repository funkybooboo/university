from typing import Tuple, Callable
import matplotlib.pyplot as plt
import numpy as np
import math
from numba import njit, prange

# Constants
NUM_PATHS: int = 5000
INITIAL_STOCK_PRICE: float = 100.0
DRIFT_RATE: float = 0.03
VOLATILITY_RATE: float = 17.04
TIME_INCREMENT: float = 1 / 365  # Daily increments
TOTAL_TIME: float = 1.0  # 1 year
STRIKE_PRICE: float = 100.0
RISK_FREE_RATE: float = 0.01
BETA_A: int = 9
BETA_B: int = 10
BETA_SHIFT: float = 0.35

def main() -> None:
    simulate_stock_and_plot("Normal", normal_price_change_generator)
    print()
    simulate_stock_and_plot("Beta", beta_price_change_generator)

def simulate_stock_and_plot(distribution_type: str, price_change_generator: Callable[[], float]) -> None:
    stock_price_paths: np.ndarray = generate_stock_price_paths(price_change_generator)

    final_prices, option_payoffs = calculate_option_pricing(stock_price_paths)

    # Plotting stock price paths
    plt.figure(figsize=(12, 6))
    for i in range(min(NUM_PATHS, 10)):
        plt.plot(stock_price_paths[i])
    plt.xlabel('Days')
    plt.ylabel('Stock Price')
    plt.title(f"Simulated Stock Price Paths Using {distribution_type} Distribution")
    plt.show()

    # Output average results
    print(f"Average stock price ({distribution_type}) after {int(1 / TIME_INCREMENT) * TOTAL_TIME} days: ${np.average(final_prices):.2f}")
    print(f"Average payoff for a block of 100 options ({distribution_type}): ${np.average(option_payoffs) * 100:.2f}")
    print(f"Estimated cost of the option ({distribution_type}): ${np.average(option_payoffs):.2f}")

@njit
def normal_price_change_generator() -> float:
    random_shock: float = np.random.normal(0, math.sqrt(TIME_INCREMENT))
    price_change: float = DRIFT_RATE * TIME_INCREMENT + VOLATILITY_RATE * random_shock
    return price_change

@njit
def beta_price_change_generator() -> float:
    random_shock: float = np.random.beta(BETA_A, BETA_B) - BETA_SHIFT
    price_change: float = DRIFT_RATE * TIME_INCREMENT + VOLATILITY_RATE * random_shock
    return price_change

@njit
def calculate_european_call_option_payoff(strike_price: float, final_stock_price: float) -> float:
    return max(final_stock_price - strike_price, 0)

@njit(parallel=True)
def generate_stock_price_paths(
        price_change_generator: Callable[[], float]
) -> np.ndarray:
    price_paths: np.ndarray = np.zeros((NUM_PATHS, int(TOTAL_TIME / TIME_INCREMENT)))
    for i in prange(NUM_PATHS):
        current_price: float = INITIAL_STOCK_PRICE
        for t in range(int(TOTAL_TIME / TIME_INCREMENT)):
            price_change: float = price_change_generator()
            current_price += price_change
            price_paths[i, t] = current_price
    return price_paths

@njit(parallel=True)
def calculate_option_pricing(
        price_paths: np.ndarray
) -> Tuple[np.ndarray, np.ndarray]:
    option_payoffs: np.ndarray = np.zeros(price_paths.shape[0])
    final_prices: np.ndarray = np.zeros(price_paths.shape[0])

    for i in prange(price_paths.shape[0]):
        final_price: float = float(price_paths[i, -1])
        final_prices[i] = final_price
        option_payoffs[i] = calculate_european_call_option_payoff(STRIKE_PRICE, final_price) / (1 + RISK_FREE_RATE)

    return final_prices, option_payoffs

if __name__ == "__main__":
    main()
