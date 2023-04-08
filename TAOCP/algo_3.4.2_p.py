#!/usr/bin/env python3
import random
import math

# An implementation of Algorithm P
X = [ ]
X.extend(range(1, 53)) # a deck of cards

def shuffle(X):
    # P1
    t = len(X) - 1
    j = t
    
    while j > 1:
        # P2 : Generate uniformly distributed random number
        U = random.uniform(0, 1)
        
        # P3
        k = math.floor(j * U)
        store = X[j]
        X[j] = X[k]
        X[k] = store

        # P4
        j -= 1

    return(X)

shuffled_X = shuffle(X)
print(shuffled_X)
