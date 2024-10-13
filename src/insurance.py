import pandas as pd
import os


def main():
    file_path = "../insurance_data.csv"
    if not os.path.exists(file_path):
        print(f"Error: The file {file_path} does not exist.")
        return
    df = pd.read_csv(file_path)

    insurance_rate, std_dev_claims, volatility = calculate_basic_insurance_rate(df)
    print(f"Basic Insurance Rate: ${insurance_rate:.2f}")
    print(f"Standard Deviation of Claims: ${std_dev_claims:.2f}")
    print(f"Volatility of the Portfolio: {volatility:.2f}")
    print()
    print("-" * 25)
    print()

    tiered_results = calculate_tiered_insurance_rate(df)
    for result in tiered_results:
        print(f"Age Category: {result['Age Category']}")
        print(f"Tiered Insurance Rate: ${result['Tiered Insurance Rate']:.2f}")
        print(f"Standard Deviation of Claims: ${result['Standard Deviation of Claims']:.2f}")
        print(f"Volatility: {result['Volatility']:.2f}")
        print()
    print("-" * 25)
    print()

    tiered_results_with_sex = calculate_tiered_insurance_rate_with_sex(df)
    for result in tiered_results_with_sex:
        print(f"Age Category: {result['Age Category']}, Sex: {result['Sex']}")
        print(f"Tiered Insurance Rate: ${result['Tiered Insurance Rate']:.2f}")
        print(f"Standard Deviation of Claims: ${result['Standard Deviation of Claims']:.2f}")
        print(f"Volatility: {result['Volatility']:.2f}")
        print()
    print("-" * 25)
    print()

    tiered_results_by_sex = calculate_tiered_insurance_rate_by_sex(df)
    for result in tiered_results_by_sex:
        print(f"Sex: {result['Sex']}")
        print(f"Tiered Insurance Rate: ${result['Tiered Insurance Rate']:.2f}")
        print(f"Standard Deviation of Claims: ${result['Standard Deviation of Claims']:.2f}")
        print(f"Volatility: {result['Volatility']:.2f}")
        print()
    print("-" * 25)
    print()

    insurance_rate_known_claimants, std_dev_claims_known, volatility_known = calculate_insurance_rate_for_known_claimants(df)
    print(f"Insurance Rate for Known Claimants: ${insurance_rate_known_claimants:.2f}")
    print(f"Standard Deviation of Claims (Known Claimants): ${std_dev_claims_known:.2f}")
    print(f"Volatility (Known Claimants): {volatility_known:.2f}")


def calculate_basic_insurance_rate(df, profit_margin=0.11):
    valid_claims = df[df['charges'] > 0].copy()
    mean_claims = valid_claims['charges'].mean() if not valid_claims.empty else 0
    total_individuals = len(df)
    expected_claims_per_person = mean_claims * (len(valid_claims) / total_individuals) if total_individuals > 0 else 0
    insurance_rate = expected_claims_per_person * (1 + profit_margin)
    std_dev_claims = valid_claims['charges'].std() if not valid_claims.empty else 0
    volatility = std_dev_claims / mean_claims if mean_claims > 0 else float('inf')
    return insurance_rate, std_dev_claims, volatility


def calculate_tiered_insurance_rate(df, profit_margin=0.11, bins=None, labels=None):
    if labels is None:
        labels = ['18-22', '23-30', '31-48', '49+']
    if bins is None:
        bins = [17, 22, 30, 48, float('inf')]

    # Create a new column for age categories
    df['age_category'] = pd.cut(df['age'], bins=bins, labels=labels)

    # Calculate results
    results = []
    for category in labels:
        category_data = df[df['age_category'] == category]

        # Total individuals in the age category
        total_individuals = len(df[df['age_category'] == category])
        valid_claims = category_data[category_data['charges'] > 0]

        if not valid_claims.empty:
            mean_claims = valid_claims['charges'].mean()
            std_dev_claims = valid_claims['charges'].std()

            # Expected claims per person considering total individuals
            expected_claims_per_person = mean_claims * (len(valid_claims) / total_individuals)

            # Calculate the tiered insurance rate
            tiered_rate = expected_claims_per_person * (1 + profit_margin)

            # Calculate volatility
            volatility = std_dev_claims / mean_claims if mean_claims > 0 else float('inf')

            results.append({
                'Age Category': category,
                'Tiered Insurance Rate': tiered_rate,
                'Standard Deviation of Claims': std_dev_claims,
                'Volatility': volatility
            })

    return results


def calculate_tiered_insurance_rate_with_sex(df, profit_margin=0.11, bins=None, labels=None):
    if labels is None:
        labels = ['18-22', '23-30', '31-48', '49+']
    if bins is None:
        bins = [17, 22, 30, 48, float('inf')]

    # Create a new column for age categories
    df['age_category'] = pd.cut(df['age'], bins=bins, labels=labels)

    results = []

    # Group by both age category and sex
    for category in labels:
        for sex in df['sex'].unique():
            category_data = df[(df['age_category'] == category) & (df['sex'] == sex)]

            # Total individuals in the age and sex category
            total_individuals = len(df[(df['age_category'] == category) & (df['sex'] == sex)])
            valid_claims = category_data[category_data['charges'] > 0]

            if not valid_claims.empty:
                mean_claims = valid_claims['charges'].mean()
                std_dev_claims = valid_claims['charges'].std()

                # Expected claims per person considering total individuals
                expected_claims_per_person = mean_claims * (len(valid_claims) / total_individuals)

                # Calculate the tiered insurance rate
                tiered_rate = expected_claims_per_person * (1 + profit_margin)

                # Calculate volatility
                volatility = std_dev_claims / mean_claims if mean_claims > 0 else float('inf')

                results.append({
                    'Age Category': category,
                    'Sex': sex,
                    'Tiered Insurance Rate': tiered_rate,
                    'Standard Deviation of Claims': std_dev_claims,
                    'Volatility': volatility
                })

    return results


def calculate_tiered_insurance_rate_by_sex(df, profit_margin=0.11):
    results = []

    # Group by sex
    for sex in df['sex'].unique():
        sex_data = df[df['sex'] == sex]

        # Total individuals in the sex category
        total_individuals = len(sex_data)
        valid_claims = sex_data[sex_data['charges'] > 0]

        if not valid_claims.empty:
            mean_claims = valid_claims['charges'].mean()
            std_dev_claims = valid_claims['charges'].std()

            # Expected claims per person considering total individuals
            expected_claims_per_person = mean_claims * (len(valid_claims) / total_individuals)

            # Calculate the tiered insurance rate
            tiered_rate = expected_claims_per_person * (1 + profit_margin)

            # Calculate volatility
            volatility = std_dev_claims / mean_claims if mean_claims > 0 else float('inf')

            results.append({
                'Sex': sex,
                'Tiered Insurance Rate': tiered_rate,
                'Standard Deviation of Claims': std_dev_claims,
                'Volatility': volatility
            })

    return results


def calculate_insurance_rate_for_known_claimants(df, profit_margin=0.11):
    # Filter for known claimants
    claimants = df[df['charges'] > 0]

    # Calculate mean claims from claimants
    mean_claims = claimants['charges'].mean() if not claimants.empty else 0

    # Calculate insurance rate with profit margin
    insurance_rate = mean_claims * (1 + profit_margin)

    # Calculate standard deviation of claims
    std_dev_claims = claimants['charges'].std() if not claimants.empty else 0

    # Calculate volatility
    volatility = std_dev_claims / mean_claims if mean_claims > 0 else float('inf')

    return insurance_rate, std_dev_claims, volatility


if __name__ == "__main__":
    main()
