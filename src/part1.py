import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as ss
import numpy as np

# Constants
FILE_PATHS = ['../data/stock1.csv', '../data/stock2.csv']
STOCK_NAMES = ['Stock 1', 'Stock 2']
DISTRIBUTIONS = {
    'norm': ss.norm,
    'lognorm': ss.lognorm,
    'beta': ss.beta
}
COLORS = {
    'norm': 'red',
    'lognorm': 'blue',
    'beta': 'green'
}

def load_data(file_path):
    """Load stock data from a CSV file and return the 'value' column."""
    try:
        df = pd.read_csv(file_path)
        return df['value']
    except FileNotFoundError:
        print(f"File {file_path} not found.")
        return None
    except KeyError:
        print(f"'value' column not found in {file_path}.")
        return None

def normalize_data(data):
    """Normalize the data to a range of (0, 1)."""
    data_min = np.min(data)
    data_max = np.max(data)
    return (data - data_min + 1e-9) / (data_max - data_min + 1e-9)

def determine_number_of_bins(data):
    """Determine the number of bins based on the length of the data."""
    n = len(data)
    if n < 10:
        return 5  # Minimum number of bins
    elif n < 50:
        return int(np.sqrt(n))  # Square root choice
    else:
        return int(np.log2(n) + 1)  # Sturges' formula

def plot_prices(data, stock_name, number_of_bins):
    """Plot a histogram of normalized prices for the given stock."""
    plt.hist(data, density=True, bins=number_of_bins, alpha=0.5, color='g')
    plt.title(f'Histogram of {stock_name}')
    plt.xlabel('Normalized Price')
    plt.ylabel('Density')

def fit_distributions(data):
    """Fit various distributions to the normalized price data."""
    fits = {}
    for name, distribution in DISTRIBUTIONS.items():
        try:
            fits[name] = distribution.fit(data)
        except Exception as e:
            print(f"Fitting {name} distribution failed: {e}")
    return fits

def plot_fitted_distributions(data, fits):
    """Overlay fitted distributions on the histogram of normalized prices."""
    x = np.linspace(min(data), max(data), 100)

    # Loop through the fitted distributions and plot them
    for dist_name, params in fits.items():
        if dist_name in COLORS:
            plt.plot(x, DISTRIBUTIONS[dist_name].pdf(x, *params), color=COLORS[dist_name], label=f'{dist_name.capitalize()} Fit')

    plt.legend()

def goodness_of_fit(data, fits):
    """Perform goodness-of-fit tests for the fitted distributions."""
    results = {}
    for dist, params in fits.items():
        results[dist] = ss.kstest(data, dist, args=params)
    return results

def main():
    for file_path, stock_name in zip(FILE_PATHS, STOCK_NAMES):
        prices = load_data(file_path)

        if prices is None or prices.empty:
            continue

        # Normalize prices and filter out exact 0 or 1
        normalized_prices = normalize_data(prices)
        normalized_prices = normalized_prices[(normalized_prices > 0) & (normalized_prices < 1)]

        # Determine the number of bins based on the normalized prices
        number_of_bins = determine_number_of_bins(normalized_prices)

        # Plot the histogram of normalized prices with dynamic bins
        plot_prices(normalized_prices, stock_name, number_of_bins)

        # Fit distributions to normalized prices
        fits = fit_distributions(normalized_prices)

        # Plot fitted distributions
        plot_fitted_distributions(normalized_prices, fits)
        plt.show()

        # Perform goodness-of-fit tests
        ks_results = goodness_of_fit(normalized_prices, fits)

        # Identify the best fitting distribution based on the highest p-value
        best_fit = max(ks_results.values(), key=lambda x: x.pvalue)
        best_distribution = [dist for dist, result in ks_results.items() if result == best_fit][0]

        # Print results
        print(f'Stock Name: {stock_name}')
        for dist, result in ks_results.items():
            stat = result.statistic
            p_val = result.pvalue
            print(f'\t{dist.capitalize()} Fit: Statistic={stat:.4f}, p-value={p_val:.4f}')

            if dist != best_distribution:
                diff_stat = stat - best_fit.statistic
                diff_p_val = p_val - best_fit.pvalue
                print(f'\t\tDifference from Best Fit: Statistic Diff={diff_stat:.4f}, p-value Diff={diff_p_val:.4f}')
            else:
                print('\t\tBest Fit')
        print()

if __name__ == '__main__':
    main()
