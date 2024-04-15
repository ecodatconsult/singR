
#' Generic function to update information in database
# modified from revans https://stackoverflow.com/questions/70655095/how-to-efficiently-paste-many-variables-into-a-sql-query-rshiny
#' @param upload_data 'data.frame', values to be updated, does not need to
#'   include all columns in the database
#' @param con postgresqlconnection
#' @param schema 'character', the schema name to receive the updated
#'   values
#' @param table 'character', the table name to receive the updated
#'   values
#' @param idfields 'character', one or more id fields that are present
#'   in both the 'upload_data' and the database table, these cannot change
#' @param verbose 'logical', be verbose about operation, default true
#' @return logical, whether 'nrow(upload_data)' rows were affected; if anfa
#'   error occurred, it is messaged to the console and a `FALSE` is
#'   returned
db_upsert <- function(upload_data, con, schema, table, idfields, verbose = TRUE) {
  #
  #   upload_data <- data.frame(1, "a", "b", "c", "d") |>
  #       setNames(db_column_names("core", "projects"))
  #
  #   schema <- "core"
  #   table <- "projects"
  #   idfields <- "project_id"

  name <- paste0(schema, ".", table)
  if (verbose) message(Sys.time(), " upsert ", name, " with ", nrow(upload_data), " rows")
  if (any(duplicated(upload_data[idfields]))) {
    message("'upload_data' contains duplicates in the idfields, upsert will not work")
    return(FALSE)
  }
  tmp_table <- paste0("uptemp_", table, "_", sample(1e6, size = 1))
  tmp_schematable <- paste(schema, tmp_table, collapse = ".",sep = ".")
  on.exit({
    DBI::dbExecute(con, paste("drop table if exists", tmp_schematable))
  }, add = TRUE)
  DBI::dbWriteTable(con, c(schema, tmp_table), upload_data)
  cn <- colnames(upload_data)
  quotednms <- DBI::dbQuoteIdentifier(con, cn)
  notid <- DBI::dbQuoteIdentifier(con, setdiff(cn, idfields))
  qry <- sprintf(
    "INSERT INTO %s ( %s )
     SELECT %s FROM %s
     ON CONFLICT ( %s ) DO
     UPDATE SET %s",
    name, paste(quotednms, collapse = " , "),
    paste(quotednms, collapse = " , "), tmp_schematable,
    paste(DBI::dbQuoteIdentifier(con, idfields), collapse = " , "),
    paste(paste(notid, paste0("EXCLUDED.", notid), sep = "="), collapse = " , "))

  res <- tryCatch(DBI::dbExecute(con, qry), error = function(e) e)
  if (inherits(res, "error")) {
    msg <- paste("error upserting data:", conditionMessage(res))
    message(Sys.time(), " ", msg)
    ret <- FALSE
    attr(ret, "error") <- conditionMessage(res)
  } else {
    ret <- (res == nrow(upload_data))
    if (!ret) {
      msg <- paste("expecting", nrow(upload_data), "rows updated, returned", res, "rows updated")
      message(Sys.time(), " ", msg)
      attr(ret, "error") <- msg
    }
  }
  ret
}


con <- function(){
    DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                   user = "postgres",
                   password = "postgres",
                   host = "localhost",
                   port = 5432,
                   dbname = "bank_db")
}

#' function to initialize database and users based on sql script
#' @param host character, host address, default is "localhost"
#' @param database character, database name for initial connection, default is "postgres"
#' @param user, character, name of the user with rights to create user roles and database, default is "postgres"
#' @param port, numeric, port for database connection
#' @export
db_init <- function(host = "localhost", database = "postgres", user = "postgres", port = 5432){

  file <- switch(Sys.info()[["sysname"]],
                 Linux = system.file("db/db_restore_readme_linux.sql", package = "bankR"),
                 Windows = system.file("db/db_restore_readme_windows.sql", package = "bankR"),
                 Darwin = system.file("Not working on Darwin!")
  )

  psql_command <- paste("psql -h", host, "-d", database, "-U", user, "-p", port, "-a -q -f", file)

  switch(Sys.info()[["sysname"]],
         Linux = system(paste("gnome-terminal --", psql_command)),
         Windows = shell.exec(psql_command),
         Darwin = stop("Not working on Darwin!"))
}

#' function to initialize schemas and tables after ctm_db has been setup by create_ctm_
#' @param host character, host address, default is "localhost"
#' @param user character, name of the user with rights to create tables and schemas, default is "postgres"
#' @param port integer, port for database connection
#' @export
db_structure <- function(host = "localhost", user = "postgres", port = 5432){
  file <- system.file("setup_db_files/ctm_dbx.backup", package = "DMCrAI")

  psql_command <- paste("psql -d ctm_db", "-h", host, "-U", user,  "-p", port, "-f", file)

  switch(Sys.info()[["sysname"]],
         Linux = system(paste("gnome-terminal --", psql_command), wait = TRUE),
         Windows = shell.exec(psql_command),
         Darwin = stop("Not working on Darwin!"))

}
