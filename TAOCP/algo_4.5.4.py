#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

This is Algorithm C, Factoring by Addition and Subtraction, p. 388 Seminumerical Algorithms, TAOCP.

"""

import math
import matplotlib.pyplot as plt


r_list = [ ] # store the values of r as they approach zero

def factor(N):
    # C1
    a = 2 * math.floor( math.sqrt(N) ) + 1
    b = 1
    r = ( (math.floor(math.sqrt(N) ) ) ** 2) - N
    # C2
    while True:
        if (r == 0):
            largest_factor = (a - b) / 2
            return(largest_factor)
        else:
            # C3: Increase a
            r = r + a
            a = a + 2
            # C4: Increase b
            r = r - b
            b = b + 2
            # C5
            if (r > 0):
                # C4
                r = r - b
                b = b + 2
        r_list.append(r)
        print(r)

# Visualize
factor(8111)
plt.plot(r_list)
plt.savefig('factor_plot.png')
