library(mixtools)

data = read.csv("~/Downloads/QSAR-data/OX2_training_disguised.csv")

fit <- normalmixEM(data$Act)

simulated_data <- rnormmix(n = 1000, lambda = fit$lambda, mu = fit$mu, sigma = fit$sigma)

# Create a density plot for observed and simulated data
plot(density(data$Act), main = "Observed vs. Simulated Data Density")
lines(density(simulated_data), col = "red")
legend("topright", legend = c("Observed Data", "Simulated Data"), col = c("black", "red"), lty = 1)

