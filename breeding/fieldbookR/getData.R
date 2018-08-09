library(mongolite)

options(mongodb = list(
  "host" = "localhost",
  "username" = "",
  "password" = ""
))
databaseName <- "shinydatabase"
collectionName <- "breeding"

saveData <- function(data, col, db) {
  # Connect to the database
  db <- mongo(collection = col,
              url = sprintf(
                "mongodb://%s/%s",
                options()$mongodb$host,
                db))
  # Insert the data into the mongo collection as a data.frame
  data <- as.data.frame((data))
  db$insert(data)
}

loadData <- function(col,dbase) {
  # Connect to the database
  db <- mongo(collection = col,
              url = sprintf(
                "mongodb://%s/%s",
             options()$mongodb$host,
                dbase))
  # Read all the entries
  data <- db$find()
  data
}