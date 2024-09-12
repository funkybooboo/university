
import scipy
import numpy as np
import matplotlib.pyplot as plt

def beta(a, b):
    beta_dist = scipy.stats.beta(a, b)
    return beta_dist

x = np.linspace(0,1,100)

print(np.random.normal(0.5,0.5))
a_current = 1
b_current = 1

for i in range(5):
    draw = np.random.normal(0.5,0.5)
    if draw > 0.5:
        b_current += 1
    else:
        a_current += 1
    plt.plot(beta(a_current, b_current).pdf(x))
    plt.show()

