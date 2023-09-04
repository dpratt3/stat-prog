# No. 23
X = c(200, 300, 400, 600, 1000)

# Part (a): min-max normalization
X_minMax = (X - min(X)) / (max(X) - min(X))
print(X_minMax)

# Part (b): Standard deviation normalization
X_sdNorm = ( X - mean(X) ) / sd(X) 
print(X_sdNorm)