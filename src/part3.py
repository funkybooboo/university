import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as ss
import numpy as np
from typing import Tuple, List, Dict, Optional, Any
import warnings

# Constants
FILE_PATHS: List[str] = ['../data/stock1.csv', '../data/stock2.csv']
STOCK_NAMES: List[str] = ['Stock 1', 'Stock 2']
DISTRIBUTIONS: Dict[str, Any] = {
    'norm': ss.norm,
    'lognorm': ss.lognorm,
    'beta': ss.beta
}
COLORS: Dict[str, str] = {
    'norm': 'red',
    'lognorm': 'blue',
    'beta': 'green'
}

def main() -> None:
    """Main function to execute the distribution fitting and plotting."""
    stock_distributions: Dict[str, Dict[str, Any]] = {}

    for file_path, stock_name in zip(FILE_PATHS, STOCK_NAMES):
        best_fitted_distribution = process_stock(file_path, stock_name)
        if not best_fitted_distribution:
            print(f"{stock_name}: could not get best_fitted_distribution")
            continue  # Process all stocks even if one fails
        print()
        print(best_fitted_distribution)
        print()
        stock_distributions[stock_name] = best_fitted_distribution

    # TODO: Implement Monte Carlo simulation based on the fitted distributions.

def process_stock(file_path: str, stock_name: str) -> Optional[Dict[str, Any]]:
    """
    Process a single stock: load data, normalize, fit distributions, and plot results.

    Parameters:
        file_path (str): Path to the stock data file.
        stock_name (str): Name of the stock.

    Returns:
        Optional[Dict[str, Any]]: Dictionary with the best distribution name and parameters, or None if unsuccessful.
    """
    non_normalized_data = load_data(file_path)

    if non_normalized_data is None or non_normalized_data.empty:
        return None

    normalized_data = normalize_data_and_filter(non_normalized_data)
    number_of_bins = determine_number_of_bins(normalized_data)

    plot_prices(normalized_data, stock_name, number_of_bins)

    fits = fit_distributions(normalized_data, non_normalized_data)
    plot_fitted_distributions(normalized_data, fits)
    plt.show()

    return evaluate_fit_results(non_normalized_data, fits, stock_name)

def normalize_data_and_filter(data: pd.Series) -> pd.Series:
    """
    Normalize data and filter out exact 0 or 1.

    Parameters:
        data (pd.Series): The data to normalize.

    Returns:
        pd.Series: Normalized data, filtered to exclude 0 and 1.
    """
    normalized_data = normalize_data(data)
    return normalized_data[(normalized_data > 0) & (normalized_data < 1)]

def evaluate_fit_results(non_normalized_data: pd.Series, fits: Dict[str, Dict[str, Tuple]], stock_name: str) -> Optional[Dict[str, Any]]:
    """
    Evaluate the fit results and identify the best fitting distribution.

    Parameters:
        non_normalized_data (pd.Series): Non-normalized stock data.
        fits (Dict[str, Dict[str, Tuple]]): Fitted parameters for each distribution.
        stock_name (str): Name of the stock.

    Returns:
        Optional[Dict[str, Any]]: Best fitting distribution and parameters, or None if no fits.
    """
    non_normalized_ks_results = goodness_of_fit(non_normalized_data, fits)

    non_normalized_best_fit = max(non_normalized_ks_results.values(), key=lambda x: x.pvalue)
    non_normalized_best_distribution = [dist for dist, result in non_normalized_ks_results.items() if result == non_normalized_best_fit][0]

    print(f'Stock Name: {stock_name}')
    for dist, result in non_normalized_ks_results.items():
        stat = result.statistic
        p_val = result.pvalue
        print(f'\t{dist.capitalize()} Fit: Statistic={stat:.4f}, p-value={p_val:.4f}')

        if dist != non_normalized_best_distribution:
            diff_stat = stat - non_normalized_best_fit.statistic
            diff_p_val = p_val - non_normalized_best_fit.pvalue
            print(f'\t\tDifference from Best Fit: Statistic Diff={diff_stat:.4f}, p-value Diff={diff_p_val:.4f}')
        else:
            print('\t\tBest Fit')

    return {
        'name': non_normalized_best_distribution,
        'params': fits[non_normalized_best_distribution]['non_normalized']
    }

def load_data(file_path: str) -> Optional[pd.Series]:
    """Load stock data from a CSV file and return the 'value' column."""
    try:
        df: pd.DataFrame = pd.read_csv(file_path)
        return df['value']
    except FileNotFoundError:
        print(f"File {file_path} not found.")
        return None
    except KeyError:
        print(f"'value' column not found in {file_path}.")
        return None

def normalize_data(data: pd.Series) -> pd.Series:
    """Normalize the data to a range of (0, 1)."""
    data_min: float = np.min(data)
    data_max: float = np.max(data)
    return (data - data_min + 1e-9) / (data_max - data_min + 1e-9)

def determine_number_of_bins(data: pd.Series) -> int:
    """Determine the number of bins based on the length of the data."""
    n: int = len(data)
    if n < 10:
        return 5  # Minimum number of bins
    elif n < 50:
        return int(np.sqrt(n))  # Square root choice
    else:
        return int(np.log2(n) + 1)  # Sturges' formula

def plot_prices(normalized_data: pd.Series, stock_name: str, number_of_bins: int) -> None:
    """Plot a histogram of normalized prices for the given stock."""
    plt.hist(normalized_data, density=True, bins=number_of_bins, alpha=0.5, color='g')
    plt.title(f'Histogram of {stock_name}')
    plt.xlabel('Normalized Price')
    plt.ylabel('Density')

def fit_distributions(normalized_data: pd.Series, non_normalized_data: pd.Series) -> Dict[str, Dict[str, Tuple]]:
    """Fit various distributions to the normalized and non-normalized price data."""
    fits: Dict[str, Dict[str, Tuple]] = {}

    for name, distribution in DISTRIBUTIONS.items():
        fits[name] = {}

        try:
            # Fit to normalized data
            fits[name]['normalized'] = distribution.fit(normalized_data)
        except Exception as e:
            print(f"Fitting {name} distribution to normalized data failed: {e}")
            raise  # Stop execution on exception

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            try:
                # Fit to non-normalized data
                fits[name]['non_normalized'] = distribution.fit(non_normalized_data)
            except Exception as e:
                print(f"Fitting {name} distribution to non-normalized data failed: {e}")
                raise  # Stop execution on exception

    return fits

def plot_fitted_distributions(data: pd.Series, fits: Dict[str, Dict[str, Tuple]]) -> None:
    """Overlay fitted distributions on the histogram of normalized prices."""
    x: np.ndarray = np.linspace(min(data), max(data), 100)

    # Loop through the fitted distributions and plot them
    for dist_name, params in fits.items():
        if dist_name in COLORS:
            plt.plot(x, DISTRIBUTIONS[dist_name].pdf(x, *params['normalized']), color=COLORS[dist_name], label=f'{dist_name.capitalize()} Fit (Normalized)')

    plt.legend()

def goodness_of_fit(data: pd.Series, fits: Dict[str, Dict[str, Tuple]]) -> Dict[str, Any]:
    """Perform goodness-of-fit tests for the fitted distributions."""
    results: Dict[str, Any] = {}
    for dist, params in fits.items():
        results[dist] = ss.kstest(data, dist, args=params['non_normalized'])
    return results

if __name__ == '__main__':
    main()
