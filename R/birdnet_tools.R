#' run BirdNet from command line
#'
#'

#' @details
#' available command line arguments (also see https://github.com/kahst/BirdNET-Analyzer)
#' --i, Path to input file or folder. If this is a file, --o needs to be a file too.
#' --o, Path to output file or folder. If this is a file, --i needs to be a file too.
#' --lat, Recording location latitude. Set -1 to ignore.
#' --lon, Recording location longitude. Set -1 to ignore.
#' --week, Week of the year when the recording was made. Values in [1, 48] (4 weeks per month). Set -1 for year-round species list.
#' --slist, Path to species list file or folder. If folder is provided, species list needs to be named "species_list.txt". If lat and lon are provided, this list will be ignored.
#' --sensitivity, Detection sensitivity; Higher values result in higher sensitivity. Values in [0.5, 1.5]. Defaults to 1.0.
#' --min_conf, Minimum confidence threshold. Values in [0.01, 0.99]. Defaults to 0.1.
#' --overlap, Overlap of prediction segments. Values in [0.0, 2.9]. Defaults to 0.0.
#' --rtype, Specifies output format. Values in ['table', 'audacity', 'r', 'kaleidoscope', 'csv']. Defaults to 'table' (Raven selection table).
#' --threads, Number of CPU threads.
#' --batchsize, Number of samples to process at the same time. Defaults to 1.
#' --locale, Locale for translated species common names. Values in ['af', 'de', 'it', ...] Defaults to 'en'.
#' --sf_thresh, Minimum species occurrence frequency threshold for location filter. Values in [0.01, 0.99]. Defaults to 0.03.
#' --classifier, Path to custom trained classifier. Defaults to None. If set, --lat, --lon and --locale are ignored.
#' --fmin and --fmax, Minimum and maximum frequency for bandpass filter. Defaults to 0 and 15000.


run_birdnet <- function(birdnet_loc, birdnet_args){
  # birdnet_loc <- "/home/alex/BirdNET-Analyzer/"
  # birdnet_args <- list(i = here::here(), o = here::here("temp_birdnet_out.csv"))

  birdnet_cmd_files <- cmd_birdnet(
    birdnet_loc = birdnet_loc,
    birdnet_args = birdnet_args
    )

  switch(Sys.info()[['sysname']],
         Linux =  system(paste0("bash ", birdnet_cmd_files$temp_cmd)),
         Windows = shell(birdnet_cmd_files$temp_cmd),
         Darwin = stop("Darwin OS not implemented yet"))

  birdnet_output <- readr::read_delim(birdnet_cmd_files$temp_out) |>
    janitor::clean_names() |>
    dplyr::rename(data_file = begin_path) |>
    dplyr::mutate(data_file = normalizePath(data_file, winslash = "/")) |>
    dplyr::mutate(deployment_id = create_id_from_path(
      path = dirname(data_file),
      level = "deployment_id"),
      .before = "data_file") |>
    dplyr::mutate(data_id = create_id_from_path(
      path = data_file,
      level = "data_dir"),
      .before = "data_file")


  lapply(birdnet_cmd_files, file.remove)

  return(birdnet_output)
}


cmd_birdnet <- function(birdnet_loc, birdnet_args){
  cmd <-  paste0(
    "which python3\n",
    paste0("python3 ", paste0(birdnet_loc, "/analyze.py ")),

    paste0(
      "--",
      names(birdnet_args),
      " ",
      unlist(birdnet_args),
      collapse = " "
      )
  )

  temp_cmd_ext <- switch(Sys.info()[['sysname']],
                         Linux =  ".sh",
                         Windows = ".bat",
                         Darwin = stop("Darwin OS not implemented yet"))
  temp_cmd <- tempfile(pattern = "birdnet_cmd_", tmpdir = here::here(), fileext = temp_cmd_ext)

  write(cmd, temp_cmd)

  return(list(
    temp_cmd = temp_cmd,
    temp_out = birdnet_args$o
  ))
}
