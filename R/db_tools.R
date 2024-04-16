db_filter_upload <- function(upload_data, con, table, idfields, verbose = TRUE) {

    if (verbose) message(Sys.time(), " upsert ", table, " with ", nrow(upload_data), " rows")

  if(!table %in% RSQLite::dbListTables(con)){
    if(verbose) message(paste0("initializing table ", table))
    RSQLite::dbWriteTable(con, table, upload_data)
  }else{
      already_in_db <- RSQLite::dbGetQuery(con, glue::glue_sql(.con = con, "SELECT {`idfields`*} FROM {`table`}", idfields = idfields)) %>%
        apply(., 1, paste, collapse = "_")

      upload_data$id_agg <- upload_data |>
        dplyr::select(!!dplyr::sym(idfields)) |>
        apply(1, paste, collapse = "_")

      upload_data_filtered <- upload_data |>
        dplyr::filter(!id_agg %in% already_in_db)  |>
        dplyr::select(-id_agg)

      if(verbose) message(paste0("uploading ", nrow(upload_data_filtered), " rows to the database after filtering"))

      upload_data_filtered |>
        RSQLite::dbAppendTable(conn = con, name = table)
  }
}
