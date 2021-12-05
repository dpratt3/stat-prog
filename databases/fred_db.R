library(fredr)
library(RPostgreSQL)

source(".postgres_password")
source(".api_key")
fredr_set_key(key)

series = read.delim("fed_cat_ids.txt") # downloaded from https://fred.stlouisfed.org/categories/5
ids = apply(series, 2, strsplit, split = ":")
first_elt = sapply(ids[[1]], "[[", 1) # first element is unique id for table

con <- dbConnect(PostgreSQL(), 
                 user= "david", 
                 password = psql_pwd, 
                 dbname = "fred_data")

# Write all available tables
for(t in 352:length(first_elt)){
  tryCatch({
    table = fredr(series_id = first_elt[[t]])
    table = as.data.frame(table)
    dbWriteTable(con, tolower(first_elt[[t]]), table)
    print(t)
  }, error = function(e, first_elt) {
    print(e)
  })
}

# Write table with long-form descriptions 
table_desc = cbind.data.frame(tolower(first_elt), series)
dbWriteTable(con, "table_descriptions", table_desc)








