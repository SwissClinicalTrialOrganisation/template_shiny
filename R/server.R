#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session ) {
  ## Get all module names
  mod <- get_modules()

  ## To get reactive data
  data <- get_data()
  rx.data <- get_reactive_data(data = data, input = input)

  ## To get reactive REDCap data
  rc_data <- get_rc_data()
  rx.rc_data <- get_reactive_rc_data(data = rc_data, input = input)

  callModule(mod_home_server, mod$home)
  # callModule(mod_recruitment_server, mod$recruit,
  #            data.randomized = rx.data$rx_random)
  
  # # mock data
  # callModule(mod_recruitment2_server, mod$recruit2,
  #            data.randomized = rx.data$rx_random, locations = data$locations,
  #            all_data = data$randomized)
  # REDCap data
  callModule(mod_recruitment2_server, mod$recruit2,
             data.randomized = rx.rc_data$rx_random, locations = rc_data$centers,
             all_data = rc_data$randomized)
  
  # callModule(mod_recruitment_map_server, mod$recruitmap,
  #            dat = rx.data$rx_random, locations = data$locations)
  # # mock data
  # callModule(mod_recruitment_prediction_server, mod$recruitment_prediction,
  #            data.randomized = rx.data$rx_random,
  #            centers = data$centers)
  # # REDCap data
  # # callModule(mod_recruitment_prediction_server, mod$recruitment_prediction,
  # #            data.randomized = rx.rc_data$rx_random,
  # #            centers = rx.rc_data$rx_locations)
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


