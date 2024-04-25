#' Generate an ID from a given path
#'
#' This function generates an ID based on the provided path and specified levels.
#'
#' @param path The path from which to generate the ID.
#' @param ... Additional arguments to be passed to extract_path_info().
#'
#' @return An ID generated from the path.
#'
#' @examples
#'
#' # Provide a path
#' path <- "path/to/your/file"
#'
#' # Generate an ID from the path
#' id <- create_id_from_path(path)
#'
#' # View the generated ID
#' print(id)
#'
#' @export
create_id_from_path <- function(path, ...){
  path |>
    extract_path_info(...) |>
    apply(1, function(x){
      x |>
        paste(collapse = "/") |>
        digest::digest(algo = "murmur32")
    }) |>
    unlist()
}


#' Extract path information
#'
#' This function extracts information from a given path up to a specified level.
#'
#' @param path The path from which to extract information.
#' @param level The level of information to extract ("data", "data_dir", "deployment_id", "location_id", "region_id", "project_id").
#'
#' @return A data frame containing extracted information.
#'
#' @examples
#'
#' # Provide a path
#' path <- "path/to/your/file"
#'
#' # Extract information from the path up to the "data" level
#' extracted_info <- extract_path_info(path, level = "data")
#'
#' # View the extracted information
#' print(extracted_info)
#'
#' @export
extract_path_info <- function(path, level = "data"){

  levels <- c("data", "data_dir", "deployment_id", "location_id", "region_id", "project_id")

  level_shift <- which(levels == level)

  path |>
    stringr::str_split("/") |>
    lapply(function(x){
      rev(x)[1:(length(levels) + 1 - level_shift)] |>
        as.list() |>
        data.frame() |>
        setNames(levels[level_shift:length(levels)])
    }) |>
    do.call(what = rbind) |>
    dplyr::select(rev(levels)[seq(length(levels) + 1 - level_shift)])
}

