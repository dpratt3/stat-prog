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

# Problem 4: Preprocessing the data
 cor(res, use="complete.obs")
