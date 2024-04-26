#' function to calculate multiple acoustic indices
#' @param files_info vector created from exif data
#' @export
proc_acoustic <- function(files_info){
    files_info <- as.data.frame(files_info) |>
      t()
    wav <- tuneR::readWave(files_info[1,2])

    aci_min_freq <- 0
    aci_max_freq <- 48000
    aci_j = 5
    aci_fft_w = 512

    aci <- soundecology::acoustic_complexity(wav, min_freq = aci_min_freq, max_freq = aci_max_freq, j = aci_j, fft_w = aci_fft_w)[(1:4)] |>
      as.data.frame() |>
      cbind(mget(ls(pattern = "aci_"))) |>
      janitor::clean_names() %>%
      cbind(files_info, .)

    adi_max_freq <- 10000
    adi_db_threshold <- -50
    adi_freq_step = 1000
    adi_shannon = TRUE

    adi <- soundecology::acoustic_diversity(wav, max_freq = adi_max_freq, db_threshold = adi_db_threshold, freq_step = adi_freq_step, shannon = adi_shannon)[1:2] |>
      as.data.frame() |>
      cbind(mget(ls(pattern = "adi_"))) |>
      janitor::clean_names() %>%
      cbind(files_info, .)


    aei_max_freq <- 10000
    aei_db_threshold <- -50
    aei_freq_step = 1000
    aei <- soundecology::acoustic_evenness(wav) |>
      as.data.frame() |>
      cbind(mget(ls(pattern = "aei_"))) |>
      janitor::clean_names() %>%
      cbind(files_info, .)


    bai_min_freq <- 2000
    bai_max_freq <- 8000
    bai_fft_w <- 512
    bai <- soundecology::bioacoustic_index(wav) |>
      as.data.frame() |>
      cbind(mget(ls(pattern = "bai_"))) |>
      janitor::clean_names() %>%
      cbind(files_info, .)


    nsdi_fft_w <- 512
    nsdi_anthro_min = 1000
    nsdi_anthro_max = 2000
    nsdi_bio_min = 2000
    nsdi_bio_max = 10000
    nsdi <- soundecology::ndsi(wav,
                               fft_w = nsdi_fft_w,
                               anthro_min = nsdi_anthro_min,
                               anthro_max = nsdi_anthro_max,
                               bio_min = nsdi_bio_min,
                               bio_max = nsdi_bio_max) |>
      as.data.frame() |>
      cbind(mget(ls(pattern = "nsdi_"))) |>
      janitor::clean_names() %>%
      cbind(files_info, .)

    ent_wl <- 512
    ent_envt <- "hil"
    ent_msmooth = NA
    ent_ksmooth = NA
    ent <- seewave::H(wav, wl = ent_wl, envt = ent_envt) |>
      as.data.frame() |>
      setNames("entropy") |>
      cbind(mget(ls(pattern = "ent_"))) |>
      janitor::clean_names() %>%
      cbind(files_info, .)

    return(
      list(aci = aci, adi = adi, aei = aei, bai = bai, ent = ent, nsdi = nsdi)
    )
}
#' get files info for running acoustic indices analysis
#' @export
get_files_info <- function(con){
  dplyr::tbl(con, "exif_metadata") |>
    dplyr::select("data_id", "data_file") |>
    dplyr::collect() |>
    dplyr::mutate(deployment_id = create_id_from_path(dirname(dirname(data_file)), level = "deployment_id"))
}

