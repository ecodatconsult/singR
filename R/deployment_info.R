#' Extract deployment information from files in a directory
#'
#' This function reads deployment summary files from a directory, formats the data, and returns deployment information.
#'
#' @param input_dir The directory containing deployment summary files.
#'
#' @return A data frame containing formatted deployment information.
#'
#' @export
#'
deployment_info <- function(input_dir){
  # switch locale for date formatting
  deployments <- list.files(input_dir, pattern = "_Summary.txt", recursive = TRUE, full.names = TRUE) |>
    normalizePath(winslash = "/") |>
    lapply(function(x){
      format_deployment(file = x)
    }) |>
    do.call(what = rbind) |>
    dplyr::mutate(deployment_id =
                    create_id_from_path(
                      path = deployment_path,
                      level = "deployment_id"),
                  .before = "deployment_path") |>
    dplyr::mutate(deployment_name = extract_path_info(path = deployment_path,
                                                     level = "deployment_id") %>%
                   apply(., 1, paste0, collapse = "_"),
                  .before = "deployment_path"
                 )

  return(deployments)

}


#' Format deployment data from a file
#'
#' This function reads and formats deployment data from a file, specifically designed for Songmeter devices.
#'
#' @param file The path to the deployment summary file.
#' @param device The type of device used for deployment (default is "songmeter").
#'
#' @return A data frame containing formatted deployment information.

format_deployment <- function(file, device = "songmeter"){
  if(device == "songmeter"){
    old_locale <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", old_locale))

    switch(Sys.info()[["sysname"]],
           Windows = Sys.setlocale("LC_TIME","English"),
           Linux = Sys.setlocale("LC_TIME", "en_GB.UTF-8"),
           Darwin = stop("Darwin OS not supported!")
    )

    formatted_deployment <- read.csv(file) |>
      janitor::clean_names() |>
      dplyr::mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%b-%d %H:%M:%S")) |>
      dplyr::group_by((paste(lat, "_", lon)), lon, lat) |>
      dplyr::summarise(start_datetime = min(datetime),
                       end_datetime = max(datetime)) |>
      sf::st_as_sf(coords = c("lon", "lat")) |>
      dplyr::select(start_datetime, end_datetime) |>
      dplyr::mutate(deployment_path = dirname(file), .before = start_datetime) |>
      sf::st_set_crs(4326)

    return(formatted_deployment)

  }else{
    stop("no other devices than songmeter supported")
  }
}
