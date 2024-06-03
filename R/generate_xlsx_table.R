create_birdnet_workbook <- function(dir_path){
#settings_dir -> dir_path
  # Lese audiodateien ein
  wavs <- list.files(dir_path, pattern = ".wav", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)


  # extrahiere Infos aus dem Pfad
  wavs_df <- extract_path_info(wavs) |>
    dplyr::select(2,6,5) |>
    setNames(c("deployment_idlong", "wavname", "BirdnetErgebnis")) |>
    dplyr::mutate(deployment_id = stringr::str_split(deployment_idlong, "__", simplify = TRUE)[,2]) |>
    dplyr::mutate(deployment_name = stringr::str_split(deployment_idlong, "__", simplify = TRUE)[,1])

  # extrahiere weitere Infos aus dem Dateinnamen

  wavs_short <- stringr::str_remove_all(wavs, dir_path)

  wavs_df_2 <- wavs_df |>
    cbind(
      wavs_df |>
        dplyr::pull(wavname) |>
        tools::file_path_sans_ext() |>
        stringr::str_split("_", simplify = TRUE) |>
        as.data.frame() |>
        setNames(c("Konfidenz", "Nr", "Recordername", "Datum", "Uhrzeit", "Segment_Start", "Segment_Ende"))
    ) |>
    dplyr::relocate(BirdnetErgebnis, .after = "Segment_Ende") |>
    dplyr::relocate(Konfidenz, .after = "Segment_Ende") |>
    dplyr::mutate(ManuellErgebnis = NA,
                  ManuellVerhalten = NA,
                  ManuellSicher = NA,
                  Soundqualitaet = NA,
                  BearbeitetDurch = NA,
                  Kommentar = NA) |>
    #dplyr::mutate(Dateipfad_excel_win = paste0(dir_path, wavs_short)) |>
    dplyr::mutate(Dateipfad = paste0(dir_path, wavs_short)) |>
    dplyr::mutate(Dateipfad_rel = wavs_short)

  # erstelle Formel um den Pfad als Hyperlink abzurufen

  column_xl_num <- which(names(wavs_df_2) == "Dateipfad_rel")
  column_xl <- LETTERS[which(names(wavs_df_2) == "Dateipfad_rel")]
  # formula_workingdirectory <- 'SUBSTITUTE(CELL("filename"), RIGHT(CELL("filename"), LEN(CELL("filename")) - FIND("Eingabeformular_3sekunden", CELL("filename")) + 1), ""),'
  #
  # formula_workingdirectory <- paste0('SUBSTITUTE(', formula_workingdirectory, '"\'", ""),')
  #
  formula_workingdirectory <- 'SUBSTITUTE(SUBSTITUTE(CELL("filename"), RIGHT(CELL("filename"), LEN(CELL("filename")) - FIND("Eingabeformular_3sekunden", CELL("filename")) + 1), ""),"\'", ""),'

  segements_worksheet_df <- wavs_df_2 |>
    dplyr::group_split(deployment_id) |>
    lapply(function(x){
      x <- x |>
        # dplyr::mutate(Dateipfad_excel_win = paste0(
        #   'HYPERLINK(SUBSTITUTE(CONCATENATE(SUBSTITUTE(CELL("filename"), RIGHT(CELL("filename"), LEN(CELL("filename")) - FIND("[", CELL("filename")) + 1), ""),',column_xl, seq(dplyr::n())+1,'),"\\", "/"),">abspielen<")'
        #   )) |>
        dplyr::mutate(Dateipfad = paste0('HYPERLINK(SUBSTITUTE(CONCATENATE(SUBSTITUTE(SUBSTITUTE(CELL("filename"), RIGHT(CELL("filename"), LEN(CELL("filename")) - FIND("Eingabeformular_3sekunden", CELL("filename")) + 1), ""),"\'", ""),',column_xl, seq(dplyr::n())+1,'),"\\[","/"),">abspielen<")'))

      class(x$Dateipfad) <- c(class(x$Dateipfad), "formula")
      #class(x$Dateipfad_excel_win) <- c(class(x$Dateipfad_excel_win), "formula")
      return(x)
    }) |>
    lapply(as.data.frame) |>
    setNames(substr(unique(wavs_df$deployment_id),0,31))

  lname <- names(segements_worksheet_df)[1]
  wb <- openxlsx::createWorkbook(creator = "Alexander Wagner, alexander.milles@wald-rlp.de")

  openxlsx::addWorksheet(wb, "erlaubte_Werte")

  # load list of common/expected species
  common_names <- readr::read_csv(system.file("data/species_list.csv", package = "singR"))|>
    dplyr::pull(common_name)

  #system.file("species_list.csv", package = "singR")
  # define allowed values (append NA to facilitate cbind of dataframe)
  erlaubte_Werte <- list(
    ManuellErgebnis = c("Biotisch - anderer Vogel", "Biotisch - unbekannter Vogel", "Biotisch - unbekannt","Biotisch - kein Vogel", "Abiotisch - Wind", "Abiotisch - Regen", sort(unique(common_names))),
    ManuellVerhalten = c("unbekannter Ruf", "Flugruf", "Gesang", "Warnruf", "Jungvogel", "Regenruf", "Kontaktruf"),
    ManuellSicher = c("sehr sicher", "sicher", "unsicher", "sehr unsicher"),
    Soundqualitaet = c("hervorragend", "gut", "durchschnittlich", "schlecht")
  )

  # append empty values so it becomes a dataframe with equal rows per column and write to worksheet
  req_length <- lapply(erlaubte_Werte, length) |>
    unlist() |>
    max()

  lapply(erlaubte_Werte, function(x, req_length){
    c(x, rep("", req_length - length(x)))
  }, req_length = req_length) |>
    data.frame() |>
    openxlsx::writeDataTable(wb = wb, sheet = "erlaubte_Werte")

  # create worksheet with validations rule
  wb |>
    openxlsx::addWorksheet(lname)

  wb |>
    openxlsx::writeDataTable(sheet = lname,
                             segements_worksheet_df[[lname]],
                             tableStyle = "TableStyleDark2")

  value <- sprintf(paste0("'erlaubte_Werte'!$A$2:$A$", length(erlaubte_Werte$ManuellErgebnis)+1))

  openxlsx::dataValidation(wb = wb,
                           sheet = lname,
                           cols = which(names(segements_worksheet_df[[lname]]) == "ManuellErgebnis"),
                           rows = seq(nrow(segements_worksheet_df[[lname]])+1),
                           type = 'list',
                           value = value)
  openxlsx::dataValidation(wb = wb,
                           sheet = lname,
                           cols = which(names(segements_worksheet_df[[lname]]) == "ManuellVerhalten"),
                           rows = seq(nrow(segements_worksheet_df[[lname]])+1),
                           type = 'list',
                           value = paste0("'erlaubte_Werte'!$B$2:$B$", length(erlaubte_Werte$ManuellVerhalten)+1))
  openxlsx::dataValidation(wb = wb,
                           sheet = lname,
                           cols = which(names(segements_worksheet_df[[lname]]) == "ManuellSicher"),
                           rows = seq(nrow(segements_worksheet_df[[lname]])+1),
                           type = 'list',
                           value = paste0("'erlaubte_Werte'!$C$2:$C$", length(erlaubte_Werte$ManuellSicher)+1))

  openxlsx::dataValidation(wb = wb,
                           sheet = lname,
                           cols = which(names(segements_worksheet_df[[lname]]) == "Soundqualitaet"),
                           rows = seq(nrow(segements_worksheet_df[[lname]])+1),
                           type = 'list',
                           value = paste0("'erlaubte_Werte'!$D$2:$D$", length(erlaubte_Werte$Soundqualitaet)+1))


  openxlsx::saveWorkbook(wb, paste0(dir_path, "/Eingabeformular_3sekunden_Segmente_",  basename(dir_path), ".xlsx"), overwrite = TRUE)
}
