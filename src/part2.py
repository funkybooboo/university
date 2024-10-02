from typing import Tuple
import matplotlib.pyplot as plt
import numpy as np
import math
from numba import njit, prange

NUM_PATHS: int = 50000
INITIAL_STOCK_PRICE: float = 100.0
DRIFT_RATE: float = 0.01
VOLATILITY_RATE: float = 70.6
TIME_INCREMENT: float = 1 / 365
TOTAL_TIME: float = 1.0
STRIKE_PRICE: float = 100.0
RISK_FREE_RATE: float = 0.01

def main() -> None:
    stock_price_paths: np.ndarray = simulate_stock_price_paths(
        INITIAL_STOCK_PRICE, DRIFT_RATE, VOLATILITY_RATE, TIME_INCREMENT, TOTAL_TIME, NUM_PATHS
    )

    final_prices, option_payoffs = calculate_option_pricing(
        stock_price_paths, STRIKE_PRICE, RISK_FREE_RATE
    )

    for i in range(min(NUM_PATHS, 10)):
        plt.plot(stock_price_paths[i])
    plt.xlabel('Days')
    plt.ylabel('Stock Price')
    plt.title("Simulated Stock Price Paths Using Geometric Brownian Motion")
    plt.show()

    print(f"Average stock price after {int(1 / TIME_INCREMENT) * TOTAL_TIME} days: ${np.average(final_prices):.2f}")
    print(f"\nAverage payoff for a block of 100 options: ${np.average(option_payoffs) * 100:.2f}")
    print(f"Estimated cost of the option: ${np.average(option_payoffs):.2f}")

@njit
def calculate_call_option_payoff(strike_price: float, final_stock_price: float) -> float:
    return max(final_stock_price - strike_price, 0)

@njit(parallel=True)
def simulate_stock_price_paths(
        initial_price: float, drift: float, volatility: float,
        time_increment: float, total_time: float, num_paths: int
) -> np.ndarray:
    price_paths: np.ndarray = np.zeros((num_paths, int(total_time / time_increment)))
    for i in prange(num_paths):
        current_price: float = initial_price
        for t in range(int(total_time / time_increment)):
            random_shock: float = np.random.normal(0, math.sqrt(time_increment))
            price_change: float = drift * time_increment + volatility * random_shock
            current_price += price_change
            price_paths[i, t] = current_price
    return price_paths

@njit(parallel=True)
def calculate_option_pricing(
        price_paths: np.ndarray, strike_price: float, risk_free_rate: float
) -> Tuple[np.ndarray, np.ndarray]:
    option_payoffs: np.ndarray = np.zeros(price_paths.shape[0])
    final_prices: np.ndarray = np.zeros(price_paths.shape[0])

    for i in prange(price_paths.shape[0]):
        final_price: float = float(price_paths[i, -1])
        final_prices[i] = final_price
        option_payoffs[i] = calculate_call_option_payoff(strike_price, final_price) / (1 + risk_free_rate)

    return final_prices, option_payoffs

if __name__ == "__main__":
    main()
