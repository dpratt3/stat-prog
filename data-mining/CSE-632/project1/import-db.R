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

apply(min_max_res, 2, hist, breaks = 30, col = "firebrick")
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