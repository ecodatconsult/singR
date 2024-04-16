#' get deployment info
#'
#' specific function for songmeter micro txt files which looks for files for each deployment named [RECORDER_NAME]_Summary.txt


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
                  .before = "deployment_path")

  return(deployments)

}


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
      dplyr::mutate(deployment_path = file, .before = start_datetime) |>
      sf::st_set_crs(4326)

    return(formatted_deployment)

  }else{
    stop("no other devices than songmeter supported")
  }
}
