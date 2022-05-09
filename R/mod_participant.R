#' Participant information
#'
#' In development
#' @rdname mod_participant
#' @param id standard shiny id argument
#' @param label standard shiny label argument
mod_participant_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   tabPanel("Participant information",
                            "This module is under development",
                            # height = "400"
                            )
                   )
            )
          )
}

#' @rdname mod_participant
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_participant_server <- function(input, output, session, data){

  ns <- session$ns

  # message(dat())

}
