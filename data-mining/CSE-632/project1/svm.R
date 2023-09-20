require(RPostgreSQL)
require(DBI)
library(caret)
library(mgcv)

pw <- {"password"}

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, 
                 dbname = "postgres",
                 host = "localhost", 
                 port = 5432,
                 user = "postgres", 
                 password = pw)

rm(pw) # removes the password

raw_data <- dbGetQuery(con, "SELECT * FROM electricvehicle")
scaled_data <- dbGetQuery(con, "SELECT * FROM scaled_data")
scaled_imputed_data <- dbGetQuery(con, "SELECT * FROM scaled_imputed_data")

# Set a random seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
train_index <- createDataPartition(scaled_imputed_data$temps_PC1_scaled, p = 0.7, list = FALSE)
train_data <- scaled_imputed_data[train_index, ]
test_data <- scaled_imputed_data[-train_index, ]

# Load necessary packages
library(mgcv)
library(caret)
library(doParallel)

# Set the number of CPU cores for parallel processing
num_cores <- 0.25 * detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Split the data into chunks for parallel processing
# You can use a package like 'parallel' to split the data into chunks
# For example, using the 'split' function:
# train_data_chunks <- split(train_data, 1:num_cores)

# Define a custom train control object for parallel processing
train_control <- trainControl(
  method = "cv",  # Use cross-validation
  number = 5,     # Number of cross-validation folds
  allowParallel = TRUE  # Allow parallel processing
)

# Define the GAM training function
train_gam <- train(
  temps_PC1_scaled ~ .,      # Replace with your actual formula
  data = train_data,
  method = "gam",           # Use the 'gam' method
  trControl = train_control
)

# Print the results or access them as needed
print(train_gam)

# Make predictions on the test dataset
predictions <- predict(train_gam, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(mean((test_data$temps_PC1_scaled - predictions)^2))

# Print RMSE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Create a scatterplot
png("gam_performance.png", width = 800, height = 800)
ggplot(data = test_data, aes(x = temps_PC1_scaled, y = predictions)) +
  geom_point() +
  ggtitle("Actual vs. Predicted Values")

dev.off()

# Save the results to a CSV file
results_df <- data.frame(
  Actual = test_data$temps_PC1_scaled,
  Predicted = predictions
)

write.csv(results_df, file = "model_results.csv")

# Close the parallel cluster
stopCluster(cl)

# Close the database connection
dbDisconnect(con)


