#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session ) {
  
  # Define src that should be used for data
  src <- "secutrial"
  src <- "redcap"
  
  ## Get all module names
  mod <- get_modules()

  ## To get reactive data
  if(src == "redcap"){
    ## To get reactive REDCap data
    data <- get_rc_data()
    rx.data <- get_reactive_data(data = data, input = input)
  } else if(src == "secutrial"){
    # To get reactive secuTrial data
    data <- get_secu_data()
    rx.data <- get_reactive_data(data = data, input = input)
  } else {
    # custom data
    data <- get_data()
    rx.data <- get_reactive_data(data = data, input = input)
  }

  callModule(mod_home_server, mod$home)
  callModule(mod_recruitment_server, mod$recruit,
             data.randomized = rx.data$rx_random)
  # callModule(mod_recruitment_map_server, mod$recruitmap,
  #            dat = rx.data$rx_random, locations = data$locations)
  callModule(mod_recruitment2_server, mod$recruit2,
             data.randomized = rx.data$rx_random, locations = data$locations,
             all_data = data$randomized)
  # callModule(mod_recruitment_prediction_server, mod$recruitment_prediction,
  #            data.randomized = rx.data$rx_random,
  #            centers = data$centers)
  # callModule(mod_retention_server, mod$retention, data)
  # callModule(mod_consistency_server, mod$consistency, 
  #            data = rx.data$rx_consistency)
  # callModule(mod_completeness_server, mod$completeness, data = data$st_data)
  # callModule(mod_timeliness_server, mod$timeliness, data)
  # callModule(mod_queries_server, mod$queries, rx.data$rx_queries)
  # callModule(mod_visits_server, mod$visits, data)
  # callModule(mod_participant_server, mod$participant, data)
  # callModule(mod_sae_server, mod$sae, rx.data$rx_sae, data$sae)
  # callModule(mod_ae_server, mod$ae, data)
  # callModule(mod_asr_server, mod$asr, data)

}


