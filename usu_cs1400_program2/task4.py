# Nate Stott
# CS1400-MO1 XL
# Assignment 2

# get user data
employeesName = input("Enter employee's name: ")
numOfHoursWorked = int(input("Enter number of hours worked in a week: "))
hourlyPayRate = float(input("Enter houly pay rate: "))
federalTax = float(input("Enter federal tax withholding rate (ex. 0.12): "))
stateTax = float(input("Enter state tax withholding rate (ex. 0.06): "))

# Calclate
grossPay = hourlyPayRate * numOfHoursWorked
fedWithholding = grossPay * federalTax
stateWithholding = grossPay * stateTax
totalDeduction = fedWithholding + stateWithholding
netPay = grossPay - totalDeduction
employeesName = employeesName.upper()
center = "^40"
rightJustIfy = ">30"

# formating
employeesName = format(employeesName + " PAY INFORMATION", center)
numOfHoursWorked = str(format(numOfHoursWorked, ">10d"))
hourlyPayRate = str(format(hourlyPayRate, ">10.2f"))
grossPay = str(format(grossPay, "10.2f"))
federalTax = str(federalTax)
fedWithholding = str(format(fedWithholding, "10.2f"))
stateTax = str(stateTax)
stateWithholding = str(format(stateWithholding, "10.2f"))
totalDeduction = str(format(totalDeduction, "10.2f"))
netPay = str(format(netPay, "10.2f"))

# add to msg
msg = "\n"
msg += employeesName
msg += "\n"
msg += "\n"
msg += format("Pay", center)
msg += "\n"
msg += format("Hours Worked:  ", rightJustIfy)
msg += numOfHoursWorked
msg += "\n"
msg += format("Pay Rate: $", rightJustIfy)
msg += hourlyPayRate
msg += "\n"
msg += format("Gross Pay: $", rightJustIfy)
msg += grossPay
msg += "\n"
msg += "\n"
msg += format("Deductions", center)
msg += "\n"
fedMsg = "Federal Withholding ("
fedMsg += federalTax
fedMsg += "%): $"
msg += format(fedMsg, rightJustIfy)
msg += fedWithholding
msg += "\n"
staMsg = "State Withholding ("
staMsg += stateTax
staMsg += "%): $"
msg += format(staMsg, rightJustIfy)
msg += stateWithholding
msg += "\n"
msg += format("Total Deduction: $", rightJustIfy)
msg += totalDeduction
msg += "\n"
msg += "\n"
msg += format("Net Pay: $", rightJustIfy)
msg += netPay

# show info
print(msg)
