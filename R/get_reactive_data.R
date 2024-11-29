#' Get reactive data
#'
#' @param input inputs from UI
#' #@param output
#' #@param session
#' @return
#' @noRd
get_reactive_data <- function(data, input){

  # data <- get_data()

  random_period <- reactive(
    
    if(input$center != "All"){
      filter(data$randomized, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
      
    } else{
      filter(data$randomized, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
    })
  
  all_period <- reactive(

    if(input$center != "All"){
      filter(data$all, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])

    } else{
      filter(data$all, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
    })
  
  completeness_period <- reactive(
    
    if(input$center != "All"){
      map(data$completeness, ~filter(.x, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2]))
      
    } else{
      map(data$completeness, ~filter(.x, rando_date.date >= input$period[1] & rando_date.date <= input$period[2]))
    })
  
  consistency_period <- reactive(
    
    if(input$center != "All"){
      filter(data$consistency, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
      
    } else{
      filter(data$consistency, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
    })
  
  sae_period <- reactive(
    if(input$center != "All"){
      filter(data$sae, centre.short == input$center & sae_date >= input$period[1] & sae_date <= input$period[2])
      
    } else{
      filter(data$sae, sae_date >= input$period[1] & sae_date <= input$period[2])
    }
  )
  
  ae_period <- reactive(
    if(input$center != "All"){
      filter(data$ae, centre.short == input$center & ae_date >= input$period[1] & ae_date <= input$period[2])
      
    } else{
      filter(data$ae, ae_date >= input$period[1] & ae_date <= input$period[2])
    }
  )
  
  sae_st_period <- reactive(
    if(input$center != "All"){
      filter(data$sae_st, centre.short.sae == input$center & mnpaedate >= input$period[1] & mnpaedate <= input$period[2])
      
    } else{
      filter(data$sae_st, mnpaedate >= input$period[1] & mnpaedate <= input$period[2])
    }
  )
  
  ae_st_period <- reactive(
    if(input$center != "All"){
      filter(data$ae_st, centre.short == input$center & mnpaedate >= input$period[1] & mnpaedate <= input$period[2])
      
    } else{
      filter(data$ae_st, mnpaedate >= input$period[1] & mnpaedate <= input$period[2])
    }
  )
  
  locations_filtered <- reactive({
    if(input$center != "All"){
      tmp <- filter(data$locations, centre.short == input$center)
    } else {
      tmp <- data$locations
    }
    return(tmp)
  })
  
  # missing_period <- reactive(
  #   
  #   if(input$center != "All"){
  #     filter(data$missing, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  #     
  #   } else{
  #     filter(data$missing, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  #   })

  queries_period <- reactive(
    
    if(input$center != "All"){
      filter(data$queries, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
      
    } else{
      filter(data$queries, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  })


  ## Save reactive datafranes into list
  reactive_data <- list(
    rx_random = random_period,
    rx_all = all_period,
    rx_completeness = completeness_period,
    rx_consistency = consistency_period,
    rx_all = all_period,
    rx_sae = sae_period,
    rx_ae = ae_period,
    rx_sae_st = sae_st_period, 
    rx_ae_st = ae_st_period, 
    rx_queries = queries_period
    , rx_locations = locations_filtered

  )

  return(reactive_data)
}


