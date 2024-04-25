devtools::load_all()

db_location <- "/home/alex/Documents/databases/soundscape.gpkg"

sf::st_write(iris, dsn = db_location, "test_data")
sf::st_delete(layer = "test_data", dsn = db_location, driver = "GPKG")
sf::st_layers(db_location)

con <- RSQLite::dbConnect(RSQLite::SQLite(), db_location)

sf::st_layers("/home/alex/Documents/databases/soundscape.gpkg")
sf::st_read("/home/alex/Documents/databases/soundscape.gpkg", layer = "birdnet_analyze_results")
# input_dir <- system.file(package = "singR")

input_dir <- "/home/alex/Documents/Soundscape/KW2100"

# get deployment info
deployment <- deployment_info(input_dir)
#
# deployment <- deployment |>
#   dplyr::filter(stringr::str_detect(deployment_path, "KW2100_test"))

deployment_id_test <- deployment |>
  dplyr::filter(stringr::str_detect(deployment_path, "/1/"))  |>
  dplyr::pull(deployment_id)

deployment_id_test == digest::digest("KW2100_test/ADK/1/240312_240325", algo = "murmur32")

deployment |>
  db_filter_upload(con = con,
                   table = "deployments",
                   idfields = "deployment_id")

# get
song_info(input_dir) |>
  db_filter_upload(con = con,
                   table = "exif_metadata",
                   idfields = "data_id")

deployment_id_sel <- deployment$deployment_id[1]

for(deployment_id_sel in deployment$deployment_id){
  print(Sys.time())
  print(deployment_id_sel)

  deployment_sel <- deployment |>
    dplyr::filter(deployment_id == deployment_id_sel)

  coords <- deployment_sel$geometry |>
    sf::st_centroid() |>
    sf::st_coordinates()

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


  settings_dir <- paste0(deployment_dir, "/"  , birdnet_analyze_settings$analyze_settings_id)
  dir.create(settings_dir)
  segments_dir <- paste0(settings_dir, "/segments/")
  classifications_dir <- paste0(settings_dir, "/selected_Data/")
  dir.create(segments_dir)
  dir.create(classifications_dir)



  # IF THESE ARE NEW SETTINGS ...
  if(!birdnet_analyze_settings$analyze_settings_id %in% previous_settings){
    # ... RUN BIRDNET

    birdnet_analyze_results <- run_birdnet(mode = "analyze",
                                           birdnet_loc = "~/BirdNET-Analyzer/",
                                           birdnet_args = birdnet_args_analyze,
                                           settings_id = birdnet_analyze_settings$analyze_settings_id)

    birdnet_analyze_results |>
      db_filter_upload(con = con,
                       table = "birdnet_analyze_results",
                       idfields = c("analyze_settings_id", "deployment_id"))


    #
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

    # generate segements with a 1 percent chance

    message(paste0(Sys.time(), ": creating segments"))

    files_with_best_detection <- birdnet_analyze_results |>
      dplyr::group_by(common_name) |>
      dplyr::filter(confidence == max(confidence)) |>
      dplyr::filter(common_name != "nocall")

    analysis_files <-  basename(files_with_best_detection$data_file) |>
      tools::file_path_sans_ext() |>
      paste0(".Bird") |>
      sapply(function(temp_dir_deployment, pattern){
        list.files(temp_dir_deployment, pattern = pattern, recursive = TRUE, full.names = TRUE)}, temp_dir_deployment = temp_dir_deployment) |>
      lapply(function(x) x[1]) |>
      unlist()

    analysis_files2 <- list.files(temp_dir_deployment, pattern = "selection.table.txt", recursive = TRUE, full.names = TRUE)

    file.copy(paste0(temp_dir_deployment, "/Data"), settings_dir, recursive = TRUE)

    analysis_files_2_segment <-
      c(
        analysis_files,
        analysis_files2[!analysis_files2 %in% analysis_files] %>%
          sample(size = ceiling(length(.)/10))
      )

    unique_analysis_files <- analysis_files_2_segment |> unique()
    file.copy(unique_analysis_files, classifications_dir)

        run_birdnet(mode = "segments",
                birdnet_loc = "~/BirdNET-Analyzer/",
                birdnet_args = list(audio = deployment_sel$deployment_path,
                                    results = classifications_dir,
                                    o = segments_dir,
                                    min_conf = 0.2,
                                    seg_length = 4.5
                ))
          create_birdnet_workbook(dir_path = settings_dir)

  }
  }

# file.copy(db_location, "/media/alex/A3A0-D191/soundscape/soundscape.gpkg")
# dir.create("/media/alex/A3A0-D191/soundscape/results")
# file.copy("/tmp/Rtmphc8Qmt", "/media/alex/A3A0-D191/soundscape/results")
