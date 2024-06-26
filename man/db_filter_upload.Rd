% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_tools.R
\name{db_filter_upload}
\alias{db_filter_upload}
\title{Filter and upload data to a database table}
\usage{
db_filter_upload(upload_data, con, table, idfields, verbose = TRUE)
}
\arguments{
\item{upload_data}{The data frame to be uploaded to the database.}

\item{con}{The connection object to the SQLite database.}

\item{table}{The name of the table in the database.}

\item{idfields}{A character vector specifying the column names used as unique identifiers.}

\item{verbose}{Logical indicating whether to display progress messages (default is TRUE).}
}
\value{
None
}
\description{
This function filters data to be uploaded to a database table based on existing records,
then uploads the filtered data to the specified table.
}
\examples{
library(RSQLite)
library(glue)
library(dplyr)

# Establish a connection to an SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = "my_database.db")

# Create sample data frame to upload
upload_data <- data.frame(
  id = c(1, 2, 3, 4),
  name = c("John", "Jane", "Alice", "Bob"),
  age = c(30, 25, 40, 35)
)

# Create a table in the database (optional if the table doesn't exist)
dbWriteTable(con, "my_table", upload_data)

# Define idfields
idfields <- c("id")

# Call db_filter_upload function to upload data to the database
db_filter_upload(upload_data, con, "my_table", idfields, verbose = TRUE)

# Disconnect from the database
dbDisconnect(con)

}
