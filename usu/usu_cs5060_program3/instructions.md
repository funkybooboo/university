Part 1: Fitting Stock Data to Distributions (10 points)

In this section, you will work with two datasets, stock1.csv and stock2-1.csv. Your goal is to find the best-fit distribution for these two stocks' historical data and evaluate how well the distributions match the data.

    Data Collection: Load the datasets for stock1 and stock2 and plot the daily returns.
        Use Python libraries like pandas and matplotlib for data loading and visualization.

    Distribution Fitting: Fit at least two different distributions (e.g., normal, log-normal, beta) to each stock's daily returns.
        Use scipy.stats for fitting distributions.

    Goodness of Fit: Compare the fitted distributions using statistical tests like the Kolmogorov-Smirnov test or visualization techniques (e.g., overlaid histograms with fitted distributions).
        Task: Provide a plot showing the stock data overlaid with the fitted distributions.
        Task: Report on the best-fitting distributions for both stocks and the metrics used to measure the goodness of fit.

Deliverables: Python code, visualizations of the fitted distributions, and a brief explanation of the best-fit distributions.
Part 2: Monte Carlo Simulation for Vanilla European Option Pricing (10 points)

In this section, you are tasked with calculating the price of a vanilla European option based on the following characteristics of an asset:

    The asset follows a beta distribution with parameters (9, 10), shifted to the left by 0.35.
    Volatility is 17.04 
    Drift is 0.03.
    The option matures in 1 year (365 days).
    The initial stock price is $100.

    Monte Carlo Simulation: Use the Monte Carlo method to simulate at least 5000 price paths for the stock. Model the stock price using a beta distribution, adding volatility and drift to simulate daily price movements.

    Option Pricing: Calculate the payoff of the option at maturity and discount the payoff to present value using a risk-free rate of your choice.

    Task: Write Python code to simulate the 5000 Monte Carlo simulations and compute the European option price.

Deliverables: Python code for the Monte Carlo simulation and the calculated option price.
Part 3: Stochastic Jumps and Basket Option Pricing (10 points)

This section expands on the first two, introducing a basket option, where the option's value is based on two underlying assets. Using the data from stock1.csv and stock2-1.csv, you will price an option based on the performance of these two stocks.

    Distribution Fitting: Revisit the distribution fitting results from Part 1. Use the best-fitting distributions for stock1 and stock2 to model the future price paths of these assets.

    Monte Carlo Simulation: Simulate price paths for both stocks over a 1-year period. Using these simulations, calculate the option price for two scenarios:
        Scenario 1: The option pays off if it outperforms the average value of stock1 and stock2 at maturity.
        Scenario 2: The option pays off if it outperforms the maximum value of either stock1 or stock2 at maturity.

    Task: Implement the simulation and calculate the option prices for both scenarios.

Deliverables: Python code, results of the option prices for both scenarios, and a comparison of the two.
Submission Guidelines:

    Submit a Python file containing the code for all three parts.
    Ensure your code is well-commented and provides outputs as specified.
    Include any plots and visualizations required for analysis.
    Write a brief summary of your results and findings (1-2 paragraphs) at the end of each section.
