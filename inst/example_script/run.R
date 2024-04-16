devtools::load_all()

db_location <- "/home/alex/Documents/databases/soundscape.gpkg"

sf::st_write(iris, dsn = db_location, "test_data")
sf::st_delete(layer = "test_data", dsn = db_location, driver = "GPKG")
sf::st_layers(db_location)

con <- RSQLite::dbConnect(RSQLite::SQLite(), db_location)


# input_dir <- system.file(package = "singR")

input_dir <- "/home/alex/Documents/Soundscape"

# get deployment info
deployment <- deployment_info(input_dir)


deployment |>
  db_filter_upload(con = con,
                   table = "deployments",
                   idfields = "deployment_id")

# get
song_info(input_dir) |>
  db_filter_upload(con = con,
                   table = "exif_metadata",
                   idfields = "data_id")

for(deployment_id_sel in deployment$deployment_id){
  print(Sys.time())
  print(deployment_id_sel)

  deployment_sel <- deployment |>
    dplyr::filter(deployment_id == deployment_id_sel)

  coords <- deployment_sel$geometry |>
    sf::st_centroid() |>
    sf::st_coordinates()

  week <- round(48 *lubridate::week(mean(c(deployment_sel$start_datetime, deployment_sel$end_datetime))) / 52)

  temp_dir <- tempdir("birdnet")

  birdnet_args_analyze <- list(i = input_dir,
                               o = temp_dir,
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

 birdnet_analyze_settings <- birdnet_args_analyze |>
    data.frame()  |>
    dplyr::select(-i, -o) %>%
    dplyr::mutate(analyze_settings_id = apply(., 1, digest::digest, algo = "md5"),
                  .before = 1)

  birdnet_analyze_settings |>
    db_filter_upload(con = con,
                     table = "birdnet_analyze_settings",
                     idfields = "analyze_settings_id")


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
  if(runif(1) > 0.99){

    message(paste0(Sys.time(), ": creating segments"))

    run_birdnet(mode = "segments",
                birdnet_loc = "~/BirdNET-Analyzer/",
                birdnet_args = list(audio = input_dir,
                                    results = temp_dir,
                                    o = temp_dir,
                                    min_conf = 0.2,
                                    seg_length = 4.5
                ))
  }
  }




