# (b) Plot discrete X values together with computed 6- and 12-hour moving averages (MA).

library(zoo) # for time series analysis
X = ts(c(7, 8, 9, 10,9, 8, 7, 9, 11, 13, 15, 17, 16, 15, 14, 13, 12, 11, 10, 9, 7, 5, 3, 1))

# Plot of six and twelve hour moving averages
X_6hr = c(rep(0, 6), rollmean(X, 6))
X_12hr = c(rep(0, 12), rollmean(X, 12))

# (b) Plot discrete X values together with computed 6- and 12-hour moving averages
# (MA).
png('9b.png')

plot(X, main = "Original, Six, and 12 Hour Moving Averages", xlab = "Time", ylab = "Value")
lines(X_6hr, col = "red")
lines(X_12hr, col = "blue")
legend("topright", legend = c("Original Data", "6-Hour MA", "12-Hour MA"), col = c("black", "red", "blue"), lty = 1)

dev.off()

# (c) Plot time-dependent variable X and its 4-h exponential moving average (EMA).
library(TTR)
X_4hr_EMA <- ts(EMA(x = X, n = 4))

# Plot the original data and 4-hour EMA:
png('9c.png')

plot(X_4hr_EMA, main = "Original Data and 4-Hour EMA", xlab = "Time", ylab = "Value")
lines(X_4hr_EMA, col = "goldenrod")
legend("topright", legend = c("Original Data", "4-Hour EMA"), col = c("black", "goldenrod"), lty = 1)

dev.off()