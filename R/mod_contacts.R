#' UI Function
#'
#' @description
#' @rdname
#' @param id,input,output,session,label Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList

mod_contacts_ui <- function(id, label){

  ns <- NS(id)

  tabItem(tabName = label,

          h3("Contacts")
  )
}

#' Server Function
#' @rdname

mod_contacts_server <- function(input, output, session){

  ns <- session$ns


}



