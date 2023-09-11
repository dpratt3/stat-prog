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

plot(res$u_q, res$u_d)

