#' Run BirdNET Analyzer with specified mode and arguments
#'
#' This function runs BirdNET Analyzer with the specified mode ("analyze" or "embeddings")
#' and command line arguments.
#'
#' @param mode The mode in which BirdNET Analyzer should be run (such as "analyze" or "embeddings").
#' @param birdnet_loc The location of the BirdNET Analyzer scripts.
#' @param birdnet_args A list of command line arguments to be passed to BirdNET Analyzer.
#' @param settings_id An identifier for the settings used for analysis.
#'
#' @return A data frame containing the output from BirdNET Analyzer.
#'
#' @details
#' Available command line arguments:
#' - --i: Path to input file or folder. If this is a file, --o needs to be a file too.
#' - --o: Path to output file or folder. If this is a file, --i needs to be a file too.
#' - --lat: Recording location latitude. Set -1 to ignore.
#' - --lon: Recording location longitude. Set -1 to ignore.
#' - --week: Week of the year when the recording was made. Values in [1, 48] (4 weeks per month). Set -1 for year-round species list.
#' - --slist: Path to species list file or folder. If folder is provided, species list needs to be named "species_list.txt". If lat and lon are provided, this list will be ignored.
#' - --sensitivity: Detection sensitivity; Higher values result in higher sensitivity. Values in [0.5, 1.5]. Defaults to 1.0.
#' - --min_conf: Minimum confidence threshold. Values in [0.01, 0.99]. Defaults to 0.1.
#' - --overlap: Overlap of prediction segments. Values in [0.0, 2.9]. Defaults to 0.0.
#' - --rtype: Specifies output format. Values in ['table', 'audacity', 'r', 'kaleidoscope', 'csv']. Defaults to 'table' (Raven selection table).
#' - --threads: Number of CPU threads.
#' - --batchsize: Number of samples to process at the same time. Defaults to 1.
#' - --locale: Locale for translated species common names. Values in ['af', 'de', 'it', ...] Defaults to 'en'.
#' - --sf_thresh: Minimum species occurrence frequency threshold for location filter. Values in [0.01, 0.99]. Defaults to 0.03.
#' - --classifier: Path to custom trained classifier. Defaults to None. If set, --lat, --lon and --locale are ignored.
#' - --fmin and --fmax: Minimum and maximum frequency for bandpass filter. Defaults to 0 and 15000.
#'
#' @examples
#' library(dplyr)
#'
#' # Set the location of BirdNET Analyzer executable
#' birdnet_location <- "/home/user/BirdNET-Analyzer/"
#'
#' # Set command line arguments
#' birdnet_arguments <- list(
#'   i = "path/to/input/file",
#'   o = "path/to/output/folder",
#'   lat = 40.7128,
#'   lon = -74.0060,
#'   week = 23,
#'   slist = "path/to/species/list",
#'   sensitivity = 1.0,
#'   min_conf = 0.1,
#'   overlap = 0.0,
#'   rtype = "table",
#'   threads = 4,
#'   batchsize = 1,
#'   locale = "en",
#'   sf_thresh = 0.03,
#'   classifier = "path/to/custom/classifier",
#'   fmin = 0,
#'   fmax = 15000
#' )
#'
#' # Run BirdNET Analyzer in "analyze" mode
#' birdnet_output <- run_birdnet(mode = "analyze", birdnet_loc = birdnet_location, birdnet_args = birdnet_arguments)
#'
#' # View the output
#' print(birdnet_output)
#'
#' @export
#'
run_birdnet <- function(mode, birdnet_loc, birdnet_args, settings_id){

  # only keep non-NA args
  non_empty_args <- birdnet_args |>
    lapply(function(x) !is.na(x)) |>
    unlist()
  birdnet_args <- birdnet_args[non_empty_args]

  # force rtype to table
  if("rtype" %in% names(birdnet_args)){
    if(birdnet_args$r != "table"){
      warning("rtype other than table not accepted, will change argument rtype to table")
    }
  }

  if(mode == "analyze") birdnet_args$rtype <- "table"

 # generate console command
  birdnet_cmd_files <- cmd_birdnet(
    mode = mode,
    birdnet_loc = birdnet_loc,
    birdnet_args = birdnet_args
    )

  # remove cmd file on exit
  on.exit(file.remove(birdnet_cmd_files$temp_cmd))

  # run command
  switch(Sys.info()[['sysname']],
         Linux =  system(paste0("bash ", birdnet_cmd_files$temp_cmd)),
         Windows = shell(birdnet_cmd_files$temp_cmd),
         Darwin = stop("Darwin OS not implemented yet"))

  #retrieve results
  if(mode == "analyze"){
    birdnet_output <- list.files(birdnet_args$o,
                                 pattern = "selection.table.txt",
                                 recursive = TRUE,
                                 full.names = TRUE) |>
      as.list() |>
      lapply(readr::read_delim, col_types = readr::cols()) |>
      do.call(what = rbind) |>
      janitor::clean_names() |>
      dplyr::rename(data_file = begin_path) |>
      dplyr::mutate(data_file = normalizePath(data_file, winslash = "/")) |>
      dplyr::mutate(deployment_id = create_id_from_path(
        path = dirname(dirname(data_file)),
        level = "deployment_id"),
        .before = "data_file") |>
      dplyr::mutate(data_id = create_id_from_path(
        path = data_file,
        level = "data"),
        .before = "data_file") |>
      dplyr::mutate(analyze_settings_id = settings_id)

    lapply(birdnet_cmd_files, file.remove)

    return(birdnet_output)
  }

  if(mode == "embeddings"){
    birdnet_output <- list.files(birdnet_args$o,
                                 pattern = "birdnet.embeddings.txt",
                                 recursive = TRUE,
                                 full.names = TRUE) |>
      as.list() |>
      lapply(function(file){
        readr::read_delim(file,
                          col_types = readr::cols(),
                          col_names = FALSE,
                          delim = ",") |>
          tidyr::separate(X1,
                          into = c("start_s", "end_s"),
                          sep = "\\t") |>
          dplyr::mutate(data_file = stringr::str_replace(file, "birdnet.embeddings.txt", "wav"),
                        .before = 1) |>
          dplyr::mutate(deployment_id = create_id_from_path(
            path = dirname(dirname(data_file)),
            level = "deployment_id"),
            .before = "data_file") |>
          dplyr::mutate(data_id = create_id_from_path(
            path = data_file,
            level = "data"),
            .before = "data_file") |>
          dplyr::mutate(analyze_settings_id = settings_id, .before = 1)
      }) |>
      do.call(what = rbind) |>
      janitor::clean_names()

    lapply(birdnet_cmd_files, file.remove)

    return(birdnet_output)
  }

}

#' Generate command to run BirdNET Analyzer
#'
#' This function generates a command to run BirdNET Analyzer with the specified mode,
#' BirdNET location, and command line arguments.
#'
#' @param mode The mode in which BirdNET Analyzer should be run (such as "analyze" or "embeddings").
#' @param birdnet_loc The location of the BirdNET Analyzer.
#' @param birdnet_args A list of command line arguments to be passed to BirdNET Analyzer.
#'
#' @return A list containing the generated command file location and the output file path.
#'
#' @examples
#'
#' # Set the location of BirdNET Analyzer
#' birdnet_location <- "/home/user/BirdNET-Analyzer/"
#'
#' # Set command line arguments
#' birdnet_arguments <- list(
#'   i = "path/to/input/file",
#'   o = "path/to/output/folder",
#'   lat = 40.7128,
#'   lon = -74.0060,
#'   week = 23,
#'   slist = "path/to/species/list",
#'   sensitivity = 1.0,
#'   min_conf = 0.1,
#'   overlap = 0.0,
#'   rtype = "table",
#'   threads = 4,
#'   batchsize = 1,
#'   locale = "en",
#'   sf_thresh = 0.03,
#'   classifier = "path/to/custom/classifier",
#'   fmin = 0,
#'   fmax = 15000
#' )
#'
#' # Generate BirdNET command
#' birdnet_command <- cmd_birdnet(mode = "analyze", birdnet_loc = birdnet_location, birdnet_args = birdnet_arguments)
#'
#' # View the generated command and output file path
#' print(birdnet_command)
#'
#' @export
#'
cmd_birdnet <- function(mode = "analyze", birdnet_loc, birdnet_args){
  cmd <-  paste0(
    "which python3\n",
    paste0("python3 ", paste0(birdnet_loc, "/", mode, ".py ")),

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
