#' Extract metadata information from audio files in a directory
#'
#' This function uses ExifTool to extract metadata information from audio files in a directory
#' and saves the information in a CSV file.
#'
#' @param song_dir The directory containing audio files.
#' @param info_out The output file path to save the metadata information (default is a temporary CSV file).
#'
#' @return A data frame containing metadata information extracted from audio files.
#'
#' @examples
#'
#' # Provide the path to the directory containing audio files
#' song_directory <- "path/to/your/audio/files"
#'
#' # Extract metadata information
#' song_info <- song_info(song_directory)
#'
#' # View the extracted metadata information
#' print(song_info)
#'
#' @export

song_info <- function(song_dir = here::here(), info_out = tempfile("out", tmpdir = here::here(), fileext = ".csv")){
 #song_dir <- "~/Documents/Soundscape/"

 exif_init <- switch(Sys.info()[['sysname']],
                         Linux =  "exiftool",
                         Windows = paste0('"',system.file("exiftool/exiftool.exe", package = "singR"),'"'),
                         Darwin = stop("Darwin OS not implemented yet"))


  cmd <- paste0(exif_init, " -ext wav -r ", song_dir ," -csv > ", '"', normalizePath(info_out, mustWork = FALSE), '"')

  temp_bat_ext <- switch(Sys.info()[['sysname']],
                    Linux =  ".sh",
                    Windows = ".bat",
                    Darwin = stop("Darwin OS not implemented yet"))

  temp_bat <- tempfile("run_exif", tmpdir = here::here(), fileext = temp_bat_ext)
  write(cmd, temp_bat)


  switch(Sys.info()[['sysname']],
         Linux =  system(paste0("bash ", temp_bat)),
         Windows = shell(temp_bat),
         Darwin = stop("Darwin OS not implemented yet"))

  file.remove(temp_bat)

  song_info <- read.csv(normalizePath(info_out)) |>
    janitor::clean_names() |>
    dplyr::rename(data_file = source_file) |>
    dplyr::mutate(data_file = normalizePath(data_file, winslash = "/")) |>
    dplyr::mutate(data_id = create_id_from_path(
      path = data_file,
      level = "data_dir"),
      .before = "data_file")


  return(song_info)
}
