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
  rx.data <- get_reactive_data(input = input)
  data <- get_data()

  callModule(mod_home_server, mod$home)
  callModule(mod_recruitment_server, mod$recruit,
             data.randomized = rx.data$rx_random)
  callModule(mod_recruitment2_server, mod$recruit2,
             data.randomized = rx.data$rx_random, locations = data$locations,
             all_data = data$randomized)
  callModule(mod_recruitment_map_server, mod$recruitmap,
             dat = rx.data$rx_random, locations = data$locations)
  callModule(mod_recruitment_prediction_server, mod$recruitment_prediction,
             data = data$randomized)
  callModule(mod_retention_server, mod$retention, data)
  callModule(mod_completeness_server, mod$completeness, data)
  callModule(mod_consistency_server, mod$consistency, data)
  callModule(mod_timeliness_server, mod$timeliness, data)
  callModule(mod_queries_server, mod$queries, data)
  callModule(mod_visits_server, mod$visits, data)
  callModule(mod_participant_server, mod$participant, data)
  callModule(mod_sae_server, mod$sae, data)
  callModule(mod_ae_server, mod$ae, data)
  callModule(mod_asr_server, mod$asr, data)
  callModule(mod_contacts_server, mod$contacts)

}


