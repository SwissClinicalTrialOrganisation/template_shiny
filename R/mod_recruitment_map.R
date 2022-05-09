#' Recruitment UI Functions including a map of the sites
#'
#' @description This module provides a leaflet map with markers for the sites
#'   together we a recruitment plot. Clicking on the map markers makes the relevant
#'   line on the recruitment plot become thicker. Hovering over the marker also
#'   shows various information on the site.
#' @rdname mod_recruitment_map
#' @param id,input,output,session,label Internal parameters for {shiny}.
#'
#' @importFrom plotly ggplotly renderPlotly layout
#' @importFrom shiny NS tagList
#' @importFrom accrualPlot accrual_create_df gg_accrual_plot_cum accrual_table
#' @importFrom leaflet leaflet addTiles addMarkers renderLeaflet leafletOutput
#' @importFrom ggplot2 geom_step theme_bw
#' @importFrom dplyr group_by summarize n left_join
#'
mod_recruitment_map_ui <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          fluidRow(
            tabBox(width = 12,
                   title = "",
                   id = "tabset2",
                   height = "450px",
                   tabPanel("Site locations",
                            leafletOutput(ns("leaflet_map")),
                            height = "400"
                            ))
          ),

          fluidRow(
            tabBox(
              width = 12,
              title = "",
              id = "tabset1", height = "450px",
              selected = "Recruitment over time by center",
              tabPanel("Recruitment over time by center",plotlyOutput(ns('recruitplot_map'), height = "400"))

            )
          )
          )
}


#' @rdname mod_recruitment_map
#' @param input,output,session Internal parameters for {shiny}.
#' @param dat reactive recruitment data
#' @param locations dataframe containing the locations of the sites.
mod_recruitment_map_server <- function(input, output, session, dat, locations){

  ns <- session$ns

  # message(dat())

  d <- reactive({
    dat() %>%
      group_by(centre.short) %>%
      summarize(n = n(),
                first = min(rando_date.date)) %>%
      left_join(locations)
    })

  acc <- reactive({
    accrualPlot::accrual_create_df(dat()$rando_date.date,
                                   by = dat()$centre.short)
  })

  # selected site
  site_data <- reactive({
    site <- input$leaflet_map_marker_click$id
    message("site = ", site)
    if(length(site) > 0){
      x <- acc()[[site]]
      x$site <- site
      return(x)
    }
  })

  ## Recruitment plot
  output$recruitplot_map <- renderPlotly({

    p <- accrualPlot::gg_accrual_plot_cum(acc()) +
      theme_bw() +
      scale_x_date(labels = function(x) format(x, format = "%d %b %Y"))

    message("length(site_data) = ", length(site_data()))

    if(length(site_data()) > 0) p <- p + geom_step(data = site_data(),
                                            aes(x = Date, y = Cumulative),
                                            size = 2)

    plotly::ggplotly(p) %>% layout(legend = list(y = 0.5))

  })

  output$leaflet_map <- renderLeaflet({
    leaflet(d()) %>%
    addTiles() %>%
    # addCircles(radius = ~ 100 * n, popup = ~ paste("<b>", centre.short, "</b>",
    #                                                "<br/>N randomized =", n,
    #                                                "<br/>Open since", first))
    addMarkers(popup = ~ paste("<b>", centre.short, "</b>",
                                     "<br/>N randomized =", n,
                                     "<br/>Open since", first),
               layerId=~centre.short)
  })

}
