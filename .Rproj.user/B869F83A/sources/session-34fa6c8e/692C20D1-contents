create_id_from_path <- function(path, ...){
  path |>
    extract_path_info(...) |>
    apply(1, function(x){
      x |>
        paste(collapse = "_") |>
        digest::digest(algo = "md5")
    }) |>
    unlist()
}



extract_path_info <- function(path, level = "data_dir"){

  levels <- c("data_dir", "deployment_id", "location_id", "region_id", "project_id")

  level_shift <- which(levels == level)

  path |>
    dirname() |>
    stringr::str_split("/") |>
    lapply(function(x){
      rev(x)[1:(6 - level_shift)] |>
        as.list() |>
        data.frame() |>
        setNames(levels[level_shift:5])
    }) |>
    do.call(what = rbind) |>
    dplyr::select(rev(levels)[seq(6 - level_shift)])
}

