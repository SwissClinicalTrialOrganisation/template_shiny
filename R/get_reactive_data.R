#' Get reactive data
#'
#' @param data output from get_data
#' @param input inputs from UI
#' #@param output
#' #@param session
#' @return
#' @noRd
get_reactive_data <- function(data, input){

  # data <- get_data()

  # # Try out automatic
  # reactive_data <- list()
  # 
  # for(i in seq_along(data)){
  #   
  #   reactive_data[[i]] <- reactive({
  #     
  #     if(input$center != "All"){
  #       filter(data[[i]], centre.short == input$center & 
  #                rando_date.date >= input$period[1] & 
  #                rando_date.date <= input$period[2])
  #       
  #     } else{
  #       filter(data[[i]], 
  #              rando_date.date >= input$period[1] & 
  #                rando_date.date <= input$period[2])
  #     }})
  #   
  # }
  # 
  # names(reactive_data) <- names(data)
  
  random_period <- reactive({
    
    if(input$center != "All"){
      filter(data$randomized, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
      
    } else{
      filter(data$randomized, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
    }})
  
  # all_period <- reactive(
  # 
  #   if(input$center != "All"){
  #     filter(data$all, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  # 
  #   } else{
  #     filter(data$all, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  #   })
  # 
  # consistency_period <- reactive(
  #   
  #   if(input$center != "All"){
  #     filter(data$consistency, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  #     
  #   } else{
  #     filter(data$consistency, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
  #   })
  # 
  # sae_period <- reactive(
  #   if(input$center != "All"){
  #     filter(data$sae, centre.short == input$center & sae_date >= input$period[1] & sae_date <= input$period[2])
  #     
  #   } else{
  #     filter(data$sae, sae_date >= input$period[1] & sae_date <= input$period[2])
  #   }
  # )
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
# 
#   queries_period <- reactive(
#     
#     if(input$center != "All"){
#       filter(data$queries, centre.short == input$center & rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
#       
#     } else{
#       filter(data$queries, rando_date.date >= input$period[1] & rando_date.date <= input$period[2])
#   })


  ## Save reactive dataframes into list
  reactive_data <- list(
    rx_random = random_period
    # , rx_all = all_period,
    # rx_consistency = consistency_period,
    # rx_all = all_period,
    # rx_sae = sae_period,
    # rx_queries = queries_period
    , rx_locations = locations_filtered

  )

  return(reactive_data)
}


