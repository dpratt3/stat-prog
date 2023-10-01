library(FSelector)

# Format given dataset
data <- data.frame(
  X1 = c(2.7, 3.1, 4.5, 5.3, 6.6, 5.0),
  X2 = c(3.4, 6.2, 2.8, 5.8, 3.1, 4.1),
  X3 = c(1, 2, 1, 2, 1, 2),
  X4 = c("A", "A", "B", "B", "A", "B")
)

# Separate the features and labels
features <- data[, 1:3]
labels <- data$X4

# Treat X4 and X3 as categorical variables
data$X4 <- factor(data$X4)
data$X3 <- factor(data$X3)

# Create a data frame with the features and labels
data_with_labels <- cbind(features, labels)
print(data_with_labels)

# Use the 'relief' function from FSelector for feature selection
relief_result <- relief(labels ~ ., data = data_with_labels)

# Print the feature importance scores
print(relief_result)

