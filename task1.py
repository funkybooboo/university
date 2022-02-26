# Nate Stott
# CS1400-MO1 XL
# Assignment 2

# get user data
InvestmentAmount = float(input("Enter the starting investment amount: "))
monthlyPaymentAmount = float(input("Enter the monthly payment amount: "))
annualInterestRate = float(input("Enter annual interest rate (ex. 4.25): "))
numOfYears = float(input("Enter number of years: "))

# adjust numbers for calculation
monthlyInterestRate = annualInterestRate / 12
monthlyInterestRate /= 100
numOfMonths = numOfYears * 12

# do calculation
futureValue = InvestmentAmount * ((1 + monthlyInterestRate) ** numOfMonths) + monthlyPaymentAmount * ((((1 + monthlyInterestRate) ** numOfMonths) - 1) / monthlyInterestRate) * (1 + monthlyInterestRate)
futureValue = round(futureValue, 2)

# give user awnser
print("Future value is: ", futureValue)
