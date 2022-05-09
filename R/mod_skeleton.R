#' UI Function
#'
#' @description
#' @rdname
#' @param id,input,output,session,label Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList

mod_skeleton_ui <- function(id, label){

  ns <- NS(id)

  tabItem(tabName = label,

  )
}

#' Server Function
#' @rdname

mod_skeleton_server <- function(input, output, session){

  ns <- session$ns


}



