#' Serious Adverse Events
#'
#' In development
#' @rdname mod_sae
#' @param id standard shiny id argument
#' @param label standard shiny label argument
mod_sae_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   tabPanel("Serious Adverse Events",
                            "This module is under development",
                            # height = "400"
                            )
                   )
            )
          )
}

#' @rdname mod_sae
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_sae_server <- function(input, output, session, data){

  ns <- session$ns

  # message(dat())

}
