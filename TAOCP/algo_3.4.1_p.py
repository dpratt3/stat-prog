#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jul 18 11:24:48 2020

This is Algorithm P, page 122, Seminumerical Algorithms, TAOCP, 3rd ed.

In the index, it appears as Algorithm 3.4.1P, 122 

"""

from numpy import random
import matplotlib.pyplot as plt
import math
import pandas as pd
from scipy import stats

def normal_deviate():
    S = 1 # initialize
    while(S >= 1):

        # P1
        U_1 = float(random.uniform(size = 1))
        U_2 = float(random.uniform(size = 1))
        V_1 = 2 * U_1 - 1
        V_2 = 2 * U_2 - 1

        # P2
        S = V_1 ** 2 + V_2 ** 2

    # P3
    X_1 = V_1 * math.sqrt(-2 * math.log(S) / S )
    X_2 = V_2 * math.sqrt(-2 * math.log(S) / S)
    return([X_1, X_2])

# Fill list with normal deviates
deviate_list = [ ]
for i in range(0, 10000):
    deviate_list.append(normal_deviate())

deviate_frame = pd.DataFrame(deviate_list)

# Plot the results
plt.hist(deviate_frame[0], color = "gold", bins = 30, density = 1)
plt.hist(deviate_frame[1], color = "red", bins = 30, density = 0.5)

# Save the plot 
plt.savefig('norm_dev_plot.png')
