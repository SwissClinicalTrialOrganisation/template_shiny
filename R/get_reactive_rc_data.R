#' Get reactive REDCap data
#'
#' @param data REDCap data
#' @param input inputs from UI
#' #@param output
#' #@param session
#' @return reactive data
#' @noRd
get_reactive_rc_data <- function(data, input){
  
  random_period <- reactive({
    if(input$center != "All"){
      filter(data$randomized, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])

    } else{
      filter(data$randomized, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
    }
  })
  
  elig_period <- reactive({
    if(input$center != "All"){
      filter(data$eligibility, centre.short == input$center & ic_date_date >= input$period[1] & ic_date_date <= input$period[2])

    } else{
      filter(data$eligibility, ic_date_date >= input$period[1] & ic_date_date <= input$period[2])
    }
  })
  
  locations_filtered <- reactive({
    if(input$center != "All"){
      tmp <- filter(data$centers_overall, centre.short == input$center)
    } else {
      tmp <- data$centers_overall
    }
    return(tmp)
  })
  
  ## Save reactive datafranes into list
  reactive_rc_data <- list(
    rx_random = random_period,
    rx_eligibility = elig_period
    # , rx_all = all_period,
    # rx_consistency = consistency_period,
    # rx_all = all_period,
    # rx_sae = sae_period,
    # rx_queries = queries_period
    , rx_locations = locations_filtered
  )

  return(reactive_rc_data)

}
