require(RPostgreSQL)
require(DBI)
library(caret)
library(mgcv)
library(doParallel)
library(ROSE)
library(corrplot)

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

# create correlograms for scaled data and imputed data

scaled_cor_mat = cor(scaled_data, use="complete.obs") # exlclude profile
scaled_imputed_cor_mat = cor(scaled_data, use="complete.obs") # exlclude profile

# save the correlogram as a PNG
png("scaled-cor-mat.png", width = 800, height = 800) 

corrplot(scaled_cor_mat, method = "color", type = "upper", order = "original",
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("blue", "orange"))(100))

title("Scaled Correlation Plot", cex.main = 2.0)
dev.off()

# save the correlogram as a PNG
png("scaled-imputed-cor-mat.png", width = 800, height = 800) 

corrplot(scaled_cor_mat, method = "color", type = "upper", order = "original",
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("blue", "orange"))(100))

title("Scaled Imputed Correlation Plot", cex.main = 2.0)
dev.off()

# Graphically compare imputed to non imputed
i_d_missing_idx = which(is.na(scaled_data$i_d))

# Set a random seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
train_index <- createDataPartition(scaled_imputed_data$temps_PC1_scaled, p = 0.7, list = FALSE)
train_data <- scaled_imputed_data[train_index, ]
test_data <- scaled_imputed_data[-train_index, ]

# train a linear model on all data:
linear_model_all_data <- lm(temps_PC1_scaled ~ ., data=scaled_imputed_data)
png("Linear model residuals.png", width=800, height=800)
hist(linear_model_all_data$residuals, col = "goldenrod", main = "Residuals Histogram", xlab = "Residuals")
dev.off()

linear_model_trained <- lm(temps_PC1_scaled ~ ., data = train_data)
linear_predictions <- predict(linear_model_trained, newdata = test_data)
linear_model_trained_predictions <- predict(linear_model_trained, newdata = test_data)
lmt_residuals <- test_data$temps_PC1_scaled - linear_model_trained_predictions
rmse <- sqrt(mean((test_data$temps_PC1_scaled - linear_model_trained_predictions)^2))
png("Trained_LM_residuals.png", width=800, height=800)
hist(lmt_residuals, col = "goldenrod", main = "Trained Linear Model Residuals", xlab = "Residuals")
dev.off()

# Set the number of CPU cores for parallel processing
num_cores <- 12
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Define a custom train control object for parallel processing
train_control <- trainControl(
  method = "cv",  # Use cross-validation
  number = 5,     # Number of cross-validation folds
  allowParallel = TRUE  # Allow parallel processing
)

# downsample train data for use in random forests
raw_length = dim(train_data)[[1]]
train_data = train_data[sample(1:raw_length, raw_length/100,replace=FALSE), ]

# Define the GAM training function
train_gam <- train(
  temps_PC1_scaled ~ .,      # Replace with your actual formula
  data = train_data,
  method = "rf",           # Use the 'gam' method
  trControl = train_control
)

# Print the results or access them as needed
print(train_gam)

# Make predictions on the test dataset for random forests
predictions <- predict(train_gam, newdata = test_data)
residuals <- test_data$temps_PC1_scaled - predictions
png("Random_forest_residuals.png", width=800, height=800)
hist(residuals, col = "goldenrod", main = "Random Forest Residuals Histogram", xlab = "Residuals")
dev.off()

# Calculate RMSE for random forests
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
# results_df <- data.frame(
#   Actual = test_data$temps_PC1_scaled,
#   Predicted = predictions
# )

# write.csv(results_df, file = "model_results.csv")

library(ggplot2)

png("Model-Performance-Lin-RF.png", width = 1200, height = 1200)
# Create a ggplot with customized settings
ggplot(data = test_data, aes(x = temps_PC1_scaled, y = linear_model_trained_predictions)) +
  geom_point(aes(color = "Linear Regression"), alpha = 0.2, size = 1) +
  geom_point(aes(y = predictions, color = "Random Forest"), alpha = 0.2, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.5, linetype = "solid") +
  ggtitle("Model Performance for EV Engine Temperature") +
  xlab("Actual Temperature (°C)") +  # Updated x-axis label with °C
  ylab("Predicted Temperature (°C)") +  # Updated y-axis label with °C
  coord_fixed(ratio = 1, xlim = c(20, 110), ylim = c(20, 110)) +
  scale_color_manual(values = c("Linear Regression" = "purple", "Random Forest" = "goldenrod")) +
  labs(color = "Models") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

dev.off()

# Close the parallel cluster
stopCluster(cl)
gc()

# Close the database connection
dbDisconnect(con)


