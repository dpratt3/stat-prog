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


