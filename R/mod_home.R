#' UI Function
#'
#' @description
#' @rdname
#' @param id,input,output,session,label Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList

mod_home_ui <- function(id, label){

  ns <- NS(id)

  tabItem(tabName = label,
          h3("About"),
          h2("This is a template shiny app that can be used for clinical trials and research")
  )
}

#' Server Function
#' @rdname

mod_home_server <- function(input, output, session){

  ns <- session$ns


}



