#' Get reactive data
#'
#' @param input inputs from UI
#' #@param output
#' #@param session
#' @return
#' @noRd
get_reactive_data <- function(input){

  data <- get_data()

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
  

  sae_period <- reactive(
    if(input$center != "All"){
      filter(data$sae, centre.short == input$center & sae_date >= input$period[1] & sae_date <= input$period[2])
      
    } else{
      filter(data$sae, sae_date >= input$period[1] & sae_date <= input$period[2])
    }
  )

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
    rx_sae = sae_period,
    rx_queries = queries_period
  )

  return(reactive_data)
}

