#' Filter and upload data to a database table
#'
#' This function filters data to be uploaded to a database table based on existing records,
#' then uploads the filtered data to the specified table.
#'
#' @param upload_data The data frame to be uploaded to the database.
#' @param con The connection object to the SQLite database.
#' @param table The name of the table in the database.
#' @param idfields A character vector specifying the column names used as unique identifiers.
#' @param verbose Logical indicating whether to display progress messages (default is TRUE).
#'
#' @return None
#'
#' @examples
#' library(RSQLite)
#' library(glue)
#' library(dplyr)
#'
#' # Establish a connection to an SQLite database
#' con <- dbConnect(RSQLite::SQLite(), dbname = "my_database.db")
#'
#' # Create sample data frame to upload
#' upload_data <- data.frame(
#'   id = c(1, 2, 3, 4),
#'   name = c("John", "Jane", "Alice", "Bob"),
#'   age = c(30, 25, 40, 35)
#' )
#'
#' # Create a table in the database (optional if the table doesn't exist)
#' dbWriteTable(con, "my_table", upload_data)
#'
#' # Define idfields
#' idfields <- c("id")
#'
#' # Call db_filter_upload function to upload data to the database
#' db_filter_upload(upload_data, con, "my_table", idfields, verbose = TRUE)
#'
#' # Disconnect from the database
#' dbDisconnect(con)
#'
#' @export
db_filter_upload <- function(upload_data, con, table, idfields, verbose = TRUE) {

  if (verbose) message(Sys.time(), " upsert ", table, " with ", nrow(upload_data), " rows")

  if (!table %in% RSQLite::dbListTables(con)) {
    if (verbose) message(paste0("initializing table ", table))
    RSQLite::dbWriteTable(con, table, upload_data)
  } else {
    already_in_db <- RSQLite::dbGetQuery(con, glue::glue_sql(.con = con, "SELECT {`idfields`*} FROM {`table`}", idfields = idfields)) %>%
      apply(., 1, paste, collapse = "_")

    upload_data$id_agg <- upload_data |>
      dplyr::select(!!dplyr::sym(idfields)) |>
      apply(1, paste, collapse = "_")

    upload_data_filtered <- upload_data |>
      dplyr::filter(!id_agg %in% already_in_db)  |>
      dplyr::select(-id_agg)

    if (verbose) message(paste0("uploading ", nrow(upload_data_filtered), " rows to the database after filtering"))

    upload_data_filtered |>
      RSQLite::dbAppendTable(conn = con, name = table)
  }
}
