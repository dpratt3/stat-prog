library(mixtools)

set.seed(1)

data1 <- read.csv("~/Downloads/QSAR-data/OX2_training_disguised.csv")
data2 <- read.csv("~/Downloads/QSAR-data/OX2_test_disguised.csv")
data <- c(data1$Act, data2$Act)

fit <- normalmixEM(data)

simulated_data <- rnormmix(n = 1000, lambda = fit$lambda, mu = fit$mu, sigma = fit$sigma)

# Find top of graph for visualization purposes
observed_density <- density(data)
simulated_density <- density(simulated_data)
max_density <- max(c(observed_density$y, simulated_density$y))

# Create a density plot for observed and simulated data
plot(density(data), main = "Observed vs. Simulated Data Density", ylim = c(0, max_density))
lines(density(simulated_data), col = "red")
legend("topright", legend = c("Observed Data", "Simulated Data"), col = c("black", "red"), lty = 1)

