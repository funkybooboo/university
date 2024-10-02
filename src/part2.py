from typing import Tuple
import matplotlib.pyplot as plt
import numpy as np
import math
from numba import njit, prange

# Constants for the simulation
NUM_PATHS: int = 50000                  # Number of simulated price paths
INITIAL_STOCK_PRICE: float = 100.0      # Starting stock price
DRIFT_RATE: float = 0.01                 # Drift rate of the stock (expected return)
VOLATILITY_RATE: float = 70.6            # Volatility of the stock (measure of risk)
TIME_INCREMENT: float = 1 / 365          # Time increment for the simulation (1 day)
TOTAL_TIME: float = 1.0                   # Total time to simulate (1 year)
STRIKE_PRICE: float = 100.0              # Strike price of the option
RISK_FREE_RATE: float = 0.01              # Risk-free interest rate (used for discounting)

def main() -> None:
    """Main function to run the stock price simulation and option pricing."""
    # Simulate stock price paths using Geometric Brownian Motion
    stock_price_paths: np.ndarray = simulate_stock_price_paths(
        INITIAL_STOCK_PRICE, DRIFT_RATE, VOLATILITY_RATE, TIME_INCREMENT, TOTAL_TIME, NUM_PATHS
    )

    # Perform Monte Carlo pricing to estimate option value
    final_prices, option_payoffs = calculate_option_pricing(
        stock_price_paths, STRIKE_PRICE, RISK_FREE_RATE
    )

    # Plot a sample of the generated stock price paths for visualization
    for i in range(min(NUM_PATHS, 10)):  # Plot only 10 paths to avoid overcrowding
        plt.plot(stock_price_paths[i])
    plt.xlabel('Days')
    plt.ylabel('Stock Price')
    plt.title("Simulated Stock Price Paths Using Geometric Brownian Motion")
    plt.show()

    # Output results: average stock price and option payoffs
    print(f"Average stock price after {int(1 / TIME_INCREMENT) * TOTAL_TIME} days: ${np.average(final_prices):.2f}")
    print(f"\nAverage payoff for a block of 100 options: ${np.average(option_payoffs) * 100:.2f}")
    print(f"Estimated cost of the option: ${np.average(option_payoffs):.2f}")

@njit
def calculate_call_option_payoff(strike_price: float, final_stock_price: float) -> float:
    """Calculate the payoff for a European call option."""
    return max(final_stock_price - strike_price, 0)

@njit(parallel=True)
def simulate_stock_price_paths(
        initial_price: float, drift: float, volatility: float,
        dt: float, total_time: float, num_paths: int
) -> np.ndarray:
    """
    Simulate stock price paths using the Geometric Brownian Motion model.

    Parameters:
        initial_price (float): The starting price of the stock.
        drift (float): The expected return of the stock.
        volatility (float): The standard deviation of the stock's returns.
        dt (float): The time increment for the simulation.
        total_time (float): The total time period for the simulation.
        num_paths (int): The number of paths to simulate.

    Returns:
        np.ndarray: A 2D array containing simulated stock prices for each path.
    """
    price_paths: np.ndarray = np.zeros((num_paths, int(total_time / dt)))
    for i in prange(num_paths):
        current_price: float = initial_price
        for t in range(int(total_time / dt)):
            # Generate a random shock for the stock price
            random_shock: float = np.random.normal(0, math.sqrt(dt))  # Brownian motion
            # Calculate the price change based on drift and volatility
            price_change: float = drift * dt + volatility * random_shock
            current_price += price_change  # Update the current price
            price_paths[i, t] = current_price  # Store price in path
    return price_paths

@njit(parallel=True)
def calculate_option_pricing(
        price_paths: np.ndarray, strike_price: float, risk_free_rate: float
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Calculate the final prices and payoffs for the options based on the simulated paths.

    Parameters:
        price_paths (np.ndarray): A 2D array of simulated stock prices.
        strike_price (float): The strike price of the option.
        risk_free_rate (float): The risk-free interest rate for discounting.

    Returns:
        Tuple[np.ndarray, np.ndarray]: A tuple containing:
            - final_prices: The final stock prices for each path.
            - option_payoffs: The calculated payoffs for the options.
    """
    option_payoffs: np.ndarray = np.zeros(price_paths.shape[0])
    final_prices: np.ndarray = np.zeros(price_paths.shape[0])

    for i in prange(price_paths.shape[0]):
        final_price: float = float(price_paths[i, -1])  # Ensure it's a float
        final_prices[i] = final_price
        # Calculate the option payoff and discount it by the risk-free rate
        option_payoffs[i] = calculate_call_option_payoff(strike_price, final_price) / (1 + risk_free_rate)

    return final_prices, option_payoffs

if __name__ == "__main__":
    main()
