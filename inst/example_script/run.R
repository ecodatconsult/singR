db_location <- "/home/alex/Documents/databases/soundscape.gpkg"

if(!file.exists(db_location)){
  sf::st_write(iris, dsn = db_location, "test_data")
  sf::st_delete(layer = "test_data", dsn = db_location, driver = "GPKG")
  sf::st_layers(db_location)
}

con <- RSQLite::dbConnect(RSQLite::SQLite(), db_location)

sf::st_layers("/home/alex/Documents/databases/soundscape.gpkg")
# input_dir <- system.file(package = "singR")

input_dir <- "/home/alex/Documents/Soundscape/KW2100"

# get deployment info
remaining_deployments <- deployment <- deployment_info(input_dir)
#
# deployment <- deployment |>
#   dplyr::filter(stringr::str_detect(deployment_path, "KW2100_test"))

deployment_id_test <- deployment |>
  dplyr::filter(stringr::str_detect(deployment_path, "/1/"))  |>
  dplyr::pull(deployment_id)

deployment_id_test == digest::digest("KW2100/ADK/1/240312_240325", algo = "murmur32")

# get deployment information and add to database
deployment |>
  db_filter_upload(con = con,
                   table = "deployments",
                   idfields = "deployment_id")

# get exif information from files and add to database
song_info(input_dir) |>
  db_filter_upload(con = con,
                   table = "exif_metadata",
                   idfields = "data_id")



#deployment <- deployment |>
#  dplyr::filter(!paste(deployment_name, deployment_id, sep = "__") %in% basename(list.dirs(paste0(input_dir, "_birdnet"), recursive = FALSE)))

# if(!is.null(deployment_id_sel)){
#   deployment <- deployment[-seq(which(deployment$deployment_id == deployment_id_sel) - 1),]
# }
#
# deployment_id_sel <- deployment$deployment_id[1]



for(deployment_id_sel in deployment$deployment_id){
  # print current time and deployment
  print(Sys.time())
  print(deployment_id_sel)

  deployment_sel <- deployment |>
    dplyr::filter(deployment_id == deployment_id_sel) |>
    dplyr::filter(!duplicated(deployment_id))

  coords <- deployment_sel$geometry |>
    sf::st_centroid() |>
    sf::st_coordinates()

  # take average week of the deployment to parameterize the birdNET
  #TODO split records by weeks?
  week <- round(48 *lubridate::week(mean(c(deployment_sel$start_datetime, deployment_sel$end_datetime))) / 52)

  temp_dir <- tempdir()
  temp_dir_deployment <- paste0(temp_dir, "/", deployment_id_sel)

  output_dir <- paste0(dirname(input_dir), "/", basename(input_dir), "_birdnet")
  dir.create(output_dir)
  dir.create(deployment_id_sel)
  deployment_dir <- paste0(output_dir, "/", paste0(deployment_sel$deployment_name, "__", deployment_sel$deployment_id))
  dir.create(deployment_dir)

  # INITIALIZING BIRDNET ARGUMENT SETTINGS
  birdnet_args_analyze <- list(i = deployment_sel$deployment_path,
                               o = temp_dir_deployment,
                               lon = coords[1],
                               lat = coords[2],
                               week = week,
                               slist = NA,
                               min_conf = 0.2,
                               overlap = 1.5,
                               rtype = "table",
                               threads = 4,
                               batchsize = 4,
                               locale = "de",
                               sf_thresh = 0.03,
                               classifier = NA,
                               fmin = 0,
                               fmax = 15000)

  message(paste0(Sys.time(), ": running analysis"))

 # ASSIGN ID TO SETTINGS BASED ON VALUES
 birdnet_analyze_settings <- birdnet_args_analyze |>
    data.frame()  |>
    dplyr::mutate(birdnet_version = sort(basename(list.dirs("~/BirdNET-Analyzer/checkpoints/", recursive = FALSE)), decreasing = TRUE)[1]) |> #ASSUME LATEST INSTALLED VERSION IS USED
    dplyr::select(-i, -o) %>%
    dplyr::mutate(analyze_settings_id = apply(., 1, digest::digest, algo = "murmur32"),
                  .before = 1)
 # UPLOAD SETTINGS TO LOCAL DB
  birdnet_analyze_settings |>
    db_filter_upload(con = con,
                     table = "birdnet_analyze_settings",
                     idfields = "analyze_settings_id")

  # CHECK FOR PREVIOUS RUNS AND SETTINGS FOR THIS DEPLOYMENT
  if("birdnet_analyze_results" %in% sf::st_layers(db_location)[[1]]){
    previous_settings <- dplyr::tbl(con, "birdnet_analyze_results") |>
      dplyr::filter(deployment_id == deployment_id_sel) |>
      dplyr::select(analyze_settings_id) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull()
  }else{
    previous_settings <- NA
  }

  # create output folders
  settings_dir <- paste0(deployment_dir, "/"  , birdnet_analyze_settings$analyze_settings_id)
  dir.create(settings_dir)
  segments_dir <- paste0(settings_dir, "/segments/")
  classifications_dir <- paste0(settings_dir, "/selected_Data/")
  dir.create(segments_dir)
  dir.create(classifications_dir)



  # IF THESE ARE NEW SETTINGS ...
  if(!birdnet_analyze_settings$analyze_settings_id %in% previous_settings){
    message(paste0(Sys.time(), ": running analysis"))
    # ... RUN BIRDNET

    birdnet_analyze_results <- run_birdnet(mode = "analyze",
                                           birdnet_loc = "~/BirdNET-Analyzer/",
                                           birdnet_args = birdnet_args_analyze,
                                           settings_id = birdnet_analyze_settings$analyze_settings_id)

    birdnet_analyze_results |>
      db_filter_upload(con = con,
                       table = "birdnet_analyze_results",
                       idfields = c("analyze_settings_id", "deployment_id"))

    file.copy(paste0(temp_dir_deployment, "/Data"), settings_dir, recursive = TRUE)


    # Skip embeddings for now as it takes too much time
    # message(paste0(Sys.time(), ": running embeddings"))
    #
    # birdnet_args_embeddings <-  list(i = input_dir,
    #                                      o = temp_dir,
    #                                      overlap = 1.5)

    #
    #   birdnet_embeddings_settings <- birdnet_args_embeddings |>
    #     data.frame()  |>
    #     dplyr::select(-i, -o) %>%
    #     dplyr::mutate(embeddings_settings_id = apply(., 1, digest::digest, algo = "md5"),
    #                   .before = 1)
    #
    #   birdnet_embeddings_settings |>
    #     db_filter_upload(con = con,
    #                      table = "birdnet_embeddings_settings",
    #                      idfields = "embedding_settings_id",
    #                      settings_id = birdnet_embeddings_settings)
    #
    #
    #   birdnet_embeddings_results <- run_birdnet(mode = "embeddings",
    #               birdnet_loc = "~/BirdNET-Analyzer/",
    #               birdnet_args = birdnet_args_embeddings)
    #

  }else{
    message(paste0("skipping ", deployment_id_sel, "as it has been processed with the same parameters already"))
  }
  }

# generate segements with a 1 percent chance
# in addition make sure each species is represented at least once

# set false if existing segments should not be overwritten
replace_segments <- TRUE

# for testing the loop
# deployment_id_sel <- deployment$deployment_id[1]
for(deployment_id_sel in deployment$deployment_id){


  deployment_duration <- dplyr::tbl(con, "deployments") |>
    dplyr::filter(deployment_id == deployment_id_sel) |>
    dplyr::collect() |>
    dplyr::mutate(duration = round((max(end_datetime) - min(start_datetime)) / 86400 / 7)) |>
    dplyr::pull(duration)

  # get deployment name of selected deployment
  deployment_name <- dplyr::tbl(con, "deployments") |>
    dplyr::filter(deployment_id == deployment_id_sel) |>
    dplyr::distinct(deployment_name) |>
    dplyr::collect() |>
    dplyr::pull(deployment_name)

  # get deployment path of selected deployment
  deployment_path <- dplyr::tbl(con, "deployments") |>
    dplyr::filter(deployment_id == deployment_id_sel) |>
    dplyr::distinct(deployment_path) |>
    dplyr::collect() |>
    dplyr::pull(deployment_path)

  # get setting ids run for this deployment (a deployment may be processed with various parameters)
  settings_ids <- dplyr::tbl(con, "birdnet_analyze_results") |>
    dplyr::filter(deployment_id == deployment_id_sel) |>
    dplyr::distinct(analyze_settings_id, deployment_id) |>
    dplyr::collect()

  # get analysis ids
  analyze_settings_id_sel = settings_ids$analyze_settings_id[1]

  # iterate through analysis ids
  for(analyze_settings_id_sel in settings_ids$analyze_settings_id){
    # get results stored in database for certain settings and deployment
    birdnet_analyze_results <- dplyr::tbl(con, "birdnet_analyze_results") |>
      dplyr::filter(deployment_id == deployment_id_sel &
                      analyze_settings_id == analyze_settings_id_sel) |>
      dplyr::collect()

    sample_species <-
    birdnet_analyze_results |>
      dplyr::select(common_name) |>
      table() |>
      dplyr::as_tibble() |>
      dplyr::filter(n > 2) |>
      dplyr::filter(common_name != "nocall") |>
      dplyr::mutate(rank = sample(seq(dplyr::n())))

    # create output folder paths
    output_dir <- paste0(dirname(input_dir), "/", basename(input_dir), "_birdnet")
    deployment_dir <- paste0(output_dir, "/", paste0(deployment_name, "__", deployment_id_sel))
    settings_dir <- paste0(deployment_dir, "/"  , analyze_settings_id_sel)
    data_dir <- paste0(settings_dir, "/Data")
    segments_dir <- paste0(settings_dir, "/segments/")
    classifications_dir <- paste0(settings_dir, "/selected_Data/")


    analysis_files_df <- birdnet_analyze_results |>
      dplyr::group_by(common_name) |>
      dplyr::slice_sample(n = 20) |>
      dplyr::bind_rows(
        birdnet_analyze_results |>
          dplyr::group_by(common_name) |>
          dplyr::filter(confidence == max(confidence))
      ) |>
      dplyr::left_join(sample_species) |>
      dplyr::filter(!is.na(rank)) |>
      dplyr::arrange(rank) |>
      dplyr::ungroup() |>
      dplyr::mutate(end_time_s = ifelse(end_time_s %% 1 == 0, paste0(end_time_s, ".0"), end_time_s)) |>
      dplyr::mutate(begin_time_s = ifelse(begin_time_s %% 1 == 0, paste0(begin_time_s, ".0"), begin_time_s)) |>
      dplyr::mutate(segment_name = paste(tools::file_path_sans_ext(basename(data_file)), paste0(begin_time_s, "s"), paste0(end_time_s, "s.wav"), sep = "_" )) |>
      dplyr::filter(!duplicated(segment_name)) |>
      head(100 * deployment_duration)

      unique_analysis_files <- analysis_files_df |>
      dplyr::pull(data_file) |>
      basename() |>
      tools::file_path_sans_ext() |>
      paste0(".Bird") |>
      sort() |>
      sapply(function(data_dir, pattern){
        list.files(data_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)},
        data_dir = data_dir) |>
      #lapply(function(x) x[1]) |>
      unlist() |>
      unique()


    if(length(list.files(classifications_dir, pattern = "selection.table.txt") == 0) | replace_segments){

      message(paste0(Sys.time(), ": creating segments"))

      # remove previously sampled segments
      file.remove(list.files(classifications_dir, full.names = TRUE))
      file.remove(list.files(segments_dir, recursive = TRUE, full.names = TRUE, include.dirs =FALSE))
      file.remove(list.files(segments_dir, recursive = TRUE, full.names = TRUE, include.dirs =TRUE))

      file.copy(unique_analysis_files |> sort(), classifications_dir)

      run_birdnet(mode = "segments",
                  birdnet_loc = "~/BirdNET-Analyzer/",
                  birdnet_args = list(audio = deployment_path,
                                      results = classifications_dir,
                                      o = segments_dir,
                                      min_conf = 0.2,
                                      seg_length = 3,
                                      max_segments = 10000
                  ))


      wav_files <- list.files(segments_dir, recursive = TRUE, full.names = TRUE, include.dirs =FALSE)

      file.remove(wav_files[!stringr::str_detect(wav_files, pattern = paste0(analysis_files_df$segment_name, collapse = "|"))])

      selected_species <- unique(analysis_files_df$common_name)
      wav_files <- list.files(segments_dir, recursive = TRUE, full.names = TRUE, include.dirs =FALSE)
      file.remove(wav_files[!stringr::str_detect(wav_files, pattern = paste0(selected_species, collapse = "|"))])
      wav_dirs <- list.files(segments_dir, recursive = TRUE, full.names = TRUE, include.dirs =TRUE)
      file.remove(wav_dirs[!stringr::str_detect(wav_dirs, pattern = paste0(selected_species, collapse = "|"))])

      create_birdnet_workbook(dir_path = settings_dir)

    }
  }
}




# Run acoustic indices analyses (seems to have memory issues?!)

data_dirs <- dplyr::tbl(con, "exif_metadata") |>
  dplyr::select("data_id", "data_file") |>
  dplyr::collect() |>
  dplyr::mutate(data_dir = dirname(data_file)) |>
  dplyr::mutate(deployment_id = create_id_from_path(dirname(data_dir), level = "deployment_id")) |>
  dplyr::filter(!duplicated(deployment_id))

acindx_dir <- paste0(dirname(input_dir), "/", basename(input_dir), "_", "acoustic_indices")
dir.create(acindx_dir)

for(acoustic_index in c("ndsi", "acoustic_complexity", "acoustic_diversity", "acoustic_evenness", "bioacustic_index", "H")){
  dir.create(paste0(acindx_dir, "/", acoustic_index))
  for(data_dir in seq(nrow(data_dirs))){
    resultfile <- paste0(acindx_dir, "/", acoustic_index, "/", data_dirs$deployment_id[data_dir], ".csv")
    soundecology::acoustic_complexity
    soundecology::multiple_sounds(directory = data_dirs$data_dir[data_dir],
                                  resultfile = resultfile,
                                  soundindex = acoustic_index,
                                  no_cores = 1
    )
  }
}

files_info <- get_files_info(con)

output <- vector("list", length = nrow(files_info))

output_file <-
list.files(pattern = "_acousticindices_output.RData") |>
  sort() |>
  tail(1)

load(output_file)
i <- 600
# crashes after a certain number of iterations, likely a memory issue?
for(i in seq(nrow(files_info))[-seq(i)]){
  write.csv(i, file = "i_value.csv", append = FALSE)
  print(i)
  gc()
  output[[i]] <- files_info[i,] |>
    proc_acoustic()
  print(i)
  if(i %% 10 == 0) print(i)
  if(i %% 100 == 0) save(output, file = paste0(i, "_acousticindices_output.RData"))
}

# parallel run version, also crashes without results..
# cl <- snow::makeSOCKcluster(12)
# parallel::clusterEvalQ(cl, library(magrittr))
# startPar <- Sys.time()
# acindxs <- parallel::parApply(cl, data_dirs, 1, proc_acoustic)
# (endPar <- Sys.time() - startPar)
# parallel::stopCluster(cl)

# spead comparison with single process
startNoPar <- Sys.time()
acindxs <- apply(data_dirs[1:10,], 1, proc_acoustic)
lapply(acindxs, function(x) x$aci) |>
  do.call(what = rbind)
(endNoPar <- startNoPar - Sys.time())

