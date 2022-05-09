#' Recruitment prediction module
#'
#' Predicts when a trial will reach it's target recruitment, based on recruitment to date
#' @rdname mod_recruitment_prediction
#' @param id standard shiny id argument
#' @param label standard shiny label argument
mod_recruitment_prediction_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   tabPanel("Predicted end date",
                            "This module is under development",
                            height = "400"
                            )
                   )
            )
          )
}

#' @rdname mod_recruitment_prediction
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_recruitment_prediction_server <- function(input, output, session, data){

  ns <- session$ns

  # message(dat())

}
