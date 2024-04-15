devtools::load_all()


input_dir <- system.file(package = "singR")

# get deployment info
deployment <- deployment_info(input_dir)

coords <- deployment$geometry |>
  sf::st_centroid() |>
  sf::st_coordinates()

# get
metadata <- song_info(input_dir)

for(deployment_id_sel in deployment_id){
  deployment_sel <- deployment |>
    dplyr::filter(deployment_id == deployment_id_sel)

  coords <- deployment_sel$geometry |>
    sf::st_centroid() |>
    sf::st_coordinates()

  week <- round(48 *lubridate::week(mean(c(deployment_sel$start_datetime, deployment_sel$end_datetime))) / 52)

  classification <- run_birdnet(birdnet_loc = "~/BirdNET-Analyzer/",
                                birdnet_args = list(i = input_dir,
                                                    o = here::here("birdnet_out.csv"),
                                                    lon = coords[1],
                                                    lat = coords[2],
                                                    locale = "de",
                                                    sf_thresh = 0.01,
                                                    overlap = 1.5,
                                                    min_conf = 0.2,
                                                    sensitivity = 1.2,
                                                    week = week
                                )
  )

0,,}


classification
all(classification$deployment_id %in% deployment$deployment_id)
all(classification$data_id %in% metadata$data_id)



