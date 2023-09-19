require(RPostgreSQL)
require(DBI)

pw <- {"password"}

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, 
                 dbname = "postgres",
                 host = "localhost", 
                 port = 5432,
                 user = "postgres", 
                 password = pw)

rm(pw) # removes the password

print(con)

res <- dbGetQuery(con, "SELECT * FROM electricvehicle")

# Confirm dimensions and importation
dim(res)
head(res)

# plot(res$u_q, res$u_d)

# Problem 2: 
# 1. What feature has missing data? How much data is missing? How do you deal with missing data?

missingData <- function(column){
    return(sum(is.na(column)))
}

# count missting data:
apply(res, 2, missingData)


# Find percentage of missing data
print(100 * sum(is.na(res$i_d)) / length(res$i_d))

# 2. What feature is particularly noisy? What level of ambiguity is the data? How do you deal with such noise?

# to identify noise, look at the variance: 
print(apply(res, 2, var, na.rm = TRUE))

# Analysis of the noisiest feature
 apply(res, 2, hist, breaks = 30, col = "firebrick")

normalize <- function(data){
    return( (data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE) )
}

min_max <- function(data){
    return( ( data - min(data, na.rm = TRUE) )/( max(data, na.rm = TRUE) - min(data, na.rm = TRUE) ) )
}

normalized_res = apply(res, 2, normalize)
min_max_res = apply(res, 2, min_max)

# apply(min_max_res, 2, hist, breaks = 30, col = "firebrick")
print(head(res))

# Problem 2 create visualizations
library(corrplot)
correlation_matrix = cor(res[1:12], use="complete.obs") # exlclude profile

# save the correlogram as a PNG
png("correlogram.png", width = 800, height = 800) 

corrplot(correlation_matrix, method = "color", type = "upper", order = "original",
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("blue", "orange"))(100))

dev.off()

# Look at histograms for each variable
library(ggplot2)
library(gridExtra)

png("histograms.png", width = 800, height = 800) 

# Set the number of bins
num_bins <- 30

# Define a color palette with goldenrod color
hist_color <- "goldenrod"

# Create a layout for the grid
par(mfrow = c(4, 3))

# Create histograms for each variable and add titles
hist(res$u_q, col = hist_color, breaks = num_bins, main = "u_q Histogram", xlab = "u_q")
hist(res$coolant, col = hist_color, breaks = num_bins, main = "coolant Histogram", xlab = "coolant")
hist(res$stator_winding, col = hist_color, breaks = num_bins, main = "stator_winding Histogram", xlab = "stator_winding")
hist(res$u_d, col = hist_color, breaks = num_bins, main = "u_d Histogram", xlab = "u_d")
hist(res$stator_tooth, col = hist_color, breaks = num_bins, main = "stator_tooth Histogram", xlab = "stator_tooth")
hist(res$motor_speed, col = hist_color, breaks = num_bins, main = "motor_speed Histogram", xlab = "motor_speed")
hist(res$i_d, col = hist_color, breaks = num_bins, main = "i_d Histogram", xlab = "i_d")
hist(res$i_q, col = hist_color, breaks = num_bins, main = "i_q Histogram", xlab = "i_q")
hist(res$pm, col = hist_color, breaks = num_bins, main = "pm Histogram", xlab = "pm")
hist(res$stator_yoke, col = hist_color, breaks = num_bins, main = "stator_yoke Histogram", xlab = "stator_yoke")
hist(res$ambient, col = hist_color, breaks = num_bins, main = "ambient Histogram", xlab = "ambient")
hist(res$torque, col = hist_color, breaks = num_bins, main = "torque Histogram", xlab = "torque")

# Reset the layout to its default
par(mfrow = c(1, 1))

dev.off()

# Missingness Analysis
library(naniar)
missing_summary <- miss_var_summary(res)

### Hypothesis One
# Missingness is MCAR from stator_winding but systematic from i_d
library(naniar)

# Convert the variable to a data frame
stator_winding_df <- data.frame(stator_winding = res$stator_winding)

# Perform Little's MCAR test
mcar_test_result <- as.numeric(mcar_test(stator_winding_df))

# Interpret the result
if (mcar_test_result[[1]] < 0.05) {
  cat("Stator winding missingness does not not follow MCAR (p-value < 0.05)")
} else {
  cat("Stator winding missingness (p-value >= 0.05)")
}

# Convert the variable to a data frame
i_q_df <- data.frame(i_q = res$i_q)

# Perform Little's MCAR test
mcar_test_result <- as.numeric(mcar_test(i_q_df))

# Interpret the result
if (mcar_test_result[[1]] < 0.05) {
  cat("Missingness for 'i_q' does not follow MCAR (p-value < 0.05)")
} else {
  cat("Missingness for 'i_q' follows MCAR (p-value >= 0.05)")
}

# quantify missingness in i_d variable: pretty stable!
unique_profiles_ids = unique(res$profile_id)
missing_fraction = vector()
for(i in unique_profiles_ids){
    subsetted_data = subset(res, profile_id == i)
    missing_fraction[i] = sum(is.na(subsetted_data$i_d)) / length(subsetted_data$i_d)
}

# There is some missingness on stator_winding as well
na_summer = function(data) sum(is.na(data))
apply(res, 2, na_summer)

print(c("the following data is missing by profile_id", missing_fraction))
# Preprocess the data with information from the histograms

# preprocess u_q (min-max scaling)
scaled_u_q = min_max(res$u_q)

# preprocess coolant
summary(res$coolant)
scaled_coolant = (res$coolant - min(res$coolant)) / (max(res$coolant) - min(res$coolant))

# preprocess stator_winding (note: there is about 5% missingness here from one )
# summary(res$stator_winding)
# scaled_stator_winding = (res$stator_winding - min(res$stator_winding)) / (max(res$stator_winding) - min(res$stator_winding))
# missing_fraction = vector()
# for(i in unique_profiles_ids){
#     subsetted_data = subset(res, profile_id == i)
#     missing_fraction[i] = sum(is.na(subsetted_data$stator_winding)) / length(subsetted_data$stator_winding)
# }

# preprocess u_d
summary(res$u_d) # symmetric
u_d_scaled = 2 * (res$u_d - min(res$u_d)) / (max(res$u_d) - min(res$u_d)) - 1

# preprocess i_d on [0, -1]
i_d_scaled <- -(res$i_d - min(res$i_d, na.rm = TRUE)) / ( max(res$i_d, na.rm = TRUE) - min(res$i_d, na.rm=TRUE) )

# preprocess i_q
summary(res$i_q) # symmetric
i_q_scaled = 2 * (res$i_q - min(res$i_q)) / (max(res$i_q) - min(res$i_q)) - 1


# Fill in the missing data prior to PCA
library(Amelia)

# PCA temps with respect only to other temps since this is a response variable
temps = cbind.data.frame(res$pm, res$stator_tooth, res$stator_winding, res$stator_yoke)
amelia_obj <- amelia(temps)
imputed_temps = amelia_obj$imputations[[1]] # imputed data

# scale motor speed
scaled_motor_speed = min_max(res$motor_speed)

# PCA pm, stator_yoke, stator_tooth, stator_winding
temp_pca <- prcomp(imputed_temps, center = TRUE, scale = TRUE)

PC1_vector <- temp_pca$x[, 1]

# Variance explained by first principal component
print(c("total variance explained by first PCA is ", temp_pca$sdev^2 / sum(temp_pca$sdev^2) * 100))

PC1_scaled = PC1_vector * temp_pca$scale + temp_pca$center
hist(PC1_scaled * temp_pca$scale + temp_pca$center) # bring back to original scale

# Calibrate PC1 to make it intelligible (remove extreme temps)
min_desired <- min(temps, na.rm = TRUE)  # Minimum value of the desired range
max_desired <- max(temps, na.rm = TRUE)

temps_PC1_scaled <- min_desired + (max_desired - min_desired) * (PC1_scaled - min(PC1_scaled)) / (max(PC1_scaled) - min(PC1_scaled))

# preprocess ambient
ambient_scaled = (res$ambient - mean(res$ambient)) / sd(res$ambient)

# preprocess torque
summary(res$torque) # symmetric
torque_scaled = 2 * (res$torque - min(res$torque)) / (max(res$torque) - min(res$torque)) - 1

# put preprocessed data together
scaled_data = cbind.data.frame(scaled_u_q,
                               scaled_coolant,
                               u_d_scaled,
                               scaled_motor_speed,
                               i_d_scaled,
                               i_q_scaled,
                               ambient_scaled,
                               torque_scaled,
                               temps_PC1_scaled)


# original formulas
formula = lm(temps_PC1_scaled ~ ., data=scaled_data)
summary(formula)