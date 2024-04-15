#' Erstell ein batch-Datei, um den Megadetector auf dem Zielsystem laufen zu lassen
#'
#' @param pics_dir character, Verzeichnis in dem der Megadetector rekursiv Bilder detektieren und klassifizieren soll
#' @param md_out character, Verzeichnis in dem die "md_out.json" gespeichert werden soll. Per Default entspricht das dem pics_dir Verzeichnis
#' @param py_scripts_loc character, Verzeichnis in dem die für Megadetector Python-Skripte hinterlegt sind (s. DMCrAI::setup_md())
#' @param md_model_loc, character, Verzeichnis in dem Megadetector-Modell liegt (s. DMCrAI::setup_md())
#' @param force.overwrite boolean, gibt an, ob die frühere md_out.json überschrieben werden soll. Falls nicht, und sofern eine md_out.json in md_out liegt, werde frühere Klassifikationen übernommen und nur neue Bilder klassifiziert
#' @param bat_loc character, Pfad unter dem die Batch-Datei abgelegt werden soll (bspw. here::here("md.bat"))
#' @param run_info boolean, gibt an, ob Checkpoints erstellt und der Konsolenoutput während der Megadetector-Prozessierung gespeichert werden soll (TEST VERSION!)
#' @param checkpoint_freq numeric, gibt die Häufigkeit an mit der Checkpoint erstellt werden
#'
#' @export

create_md_bat <- function(pics_dir,
                          md_out = pics_dir,
                          py_scripts_loc = NULL,
                          md_model_loc = NULL,
                          force.overwrite = FALSE,
                          bat_loc = NULL,
                          run_info = FALSE,
                          checkpoint_freq = 500,
                          show_finish = TRUE){

  if(Sys.info()[['sysname']] == "Linux"){
    if(file.exists(paste0(md_out,'/md_out.json'))){
      backup_time <- strftime(lubridate::now(), format = "%y%m%d_%H%M%S")
      file.copy(paste0(md_out,'/md_out.json'), paste0(md_out,'/', backup_time, '_backup_md_out.json'))
    }


    cmd_message <-
      paste0('cd "', py_scripts_loc,'/MegaDetector"\n',
             "source ", paste(stringr::str_split(bat_loc, "/")[[1]][1:3], collapse = "/"), "/miniforge3/etc/profile.d/conda.sh\n",
             "conda activate cameratraps-detector\n",
             'export PYTHONPATH=', py_scripts_loc, '/MegaDetector:', py_scripts_loc, '/yolov5\n',
             'python detection/run_detector_batch.py "', md_model_loc, '" "', pics_dir, '" "', md_out,'/md_out.json"', " --recursive")


    if(!force.overwrite & file.exists(paste0(md_out,'/md_out.json'))) {
      cmd_message <- paste0(cmd_message, ' --resume_from_checkpoint "', paste0(md_out,'/md_out.json"'))
    }


    if(show_finish){
      cmd_message <- paste0(cmd_message, "\n",
                            paste0("echo 'This file should quickly disappear'>", paste0(md_out,'/megadetector_just_finished.txt')))
    }
  }else{
    if(file.exists(paste0(md_out,'\\md_out.json'))){
      backup_time <- strftime(lubridate::now(), format = "%y%m%d_%H%M%S")
      file.copy(paste0(md_out,'\\md_out.json'), paste0(md_out,'\\', backup_time, '_backup_md_out.json'))
    }

    cmd_message <-
      paste0(substr(py_scripts_loc, 1 ,2), "\n",
             'cd "', py_scripts_loc,'\\cameratraps"\n',
             "call activate cameratraps-detector\n",
             'set PYTHONPATH=%PYTHONPATH%;', py_scripts_loc, '\\CameraTraps;', py_scripts_loc, '\\ai4eutils;', py_scripts_loc, '\\yolov5\n',
             'python detection\\run_detector_batch.py "', md_model_loc, '" "', pics_dir, '" "', md_out,'\\md_out.json"', " --recursive")

    if(!force.overwrite & file.exists(paste0(md_out,'\\md_out.json'))) {
      cmd_message <- paste0(cmd_message, ' --resume_from_checkpoint "', paste0(md_out,'\\md_out.json"'))
    }

    if(run_info){
      cmd_message <- paste0(cmd_message, " --checkpoint_frequency ", checkpoint_freq, " --checkpoint_path ", here::here("checkpoint.json"), " >> ", here::here("run_info.txt"))
    }

    if(show_finish){
      cmd_message <- paste0(cmd_message, "\n",
                            paste0("echo This file should quickly disappear>", paste0(md_out,'\\megadetector_just_finished.txt"')))
    }
  }




  writeLines(cmd_message, bat_loc)

}
