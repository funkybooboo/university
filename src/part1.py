import pandas as pd
import matplotlib.pyplot as plt
import scipy.stats as ss

def main():
    file_paths = ['../data/stock1.csv', '../data/stock2.csv']

    for file_path in file_paths:
        # Read the CSV file into a DataFrame
        df = pd.read_csv(file_path)

        # Extract the 'value' column from the DataFrame
        data = df['value']

        # Sort the data for plotting the fitted normal distribution
        datasort = data.sort_values()

        # Fit a normal distribution to the data
        norm_fit = ss.norm.fit(data)

        # Plot the histogram of the data, normalized to form a probability density
        # density=True: Normalize the histogram such that the area under the histogram sums to 1
        # bins=100: Use 100 bins to create the histogram
        # alpha=0.5: Set the transparency level to 0.5 (semi-transparent)
        plt.hist(data, density=True, bins=100, alpha=0.5, color='g')

        # Plot the fitted normal distribution curve on top of the histogram
        plt.plot(datasort, ss.norm.pdf(datasort, *norm_fit), color='r')

        plt.show()

        print(f'file path: {file_path}')
        print('\tnormal fit:')
        print(f'\t\tmean: {norm_fit[0]}')
        print(f'\t\tstandard deviation: {norm_fit[1]}')

if __name__ == '__main__':
    main()
