# Test Muriel
#' Recruitment prediction module
#'
#' Predicts when a trial will reach it's target recruitment, based on recruitment to date
#' @rdname mod_recruitment_prediction
#' @param id standard shiny id argument
#' @param label standard shiny label argument
#' 
#' @author Muriel Helmers
#' @importFrom shiny NS tagList
#' @importFrom accrualPlot accrual_create_df gg_accrual_plot_predict accrual_table
#' @importFrom plotly ggplotly renderPlotly layout

mod_recruitment_prediction_ui <- function(id, label) {
  ns <- NS(id)
  tabItem(
    tabName = label,
    fluidRow(
      tabBox(
        width = 12,
        title = "",
        id = "tabset1", height = "450px",
        selected = "Predicted end date by site",
        tabPanel("Predicted end date by site",
                 plotlyOutput(ns("predictplot_site"), height = "400"),
                 tags$br()
        ),
        tabPanel("Predicted end date using different target definitions",
                 selectInput(ns("filter_add"), "Choose definition for accrual target", choices = c("At least 1 QoL done", "FU1 performed"), selected = "None"),
                 tags$br(),
                 plotlyOutput(ns("predictplot"), height = "400"),
                 tags$br(),
                 gt::gt_output(ns('recruittable'))
        )
      )
    )
  )
}

#' @rdname mod_recruitment_prediction
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data.randomized reactive data containing randomization info
#' @param locations parameters on site level
#' @param centers parameters on site level incl. overall (needed for gg_accrual_plot_predict with targets)
#' @param study_params general study parameter
#' @param all_data non reactive data 

mod_recruitment_prediction_server <- function(input, output, session, data.randomized, locations, centers, study_params, all_data) {
  ns <- session$ns

    acc <- reactive({
      if (nlevels(factor(data.randomized()$centre.short))==1)
      {
        # if only 1 center is chosen, accrual_create_df() should be used without the "by" parameter
        # otherwise we get in trouble with the overall df that is created even for one site
        tmp <- accrualPlot::accrual_create_df(data.randomized()$rando_date.date)
      } else {
        tmp <- accrualPlot::accrual_create_df(data.randomized()$rando_date.date,
                                            by = factor(data.randomized()$centre.short)
        )
      }
        return(tmp)
    })
  
    # accrual dataframe for tab with with different target definitions
  acc_qol <- reactive({
    if (input$filter_add == "At least 1 QoL done")
      tmp <- filter(data.randomized(), bl.qol.done == TRUE)
    if (input$filter_add == "FU1 performed")
      tmp <- filter(data.randomized(), FU1 == TRUE)
    
      if (nlevels(factor(data.randomized()$centre.short))==1)
      {
        x <- accrualPlot::accrual_create_df(tmp$rando_date.date)
      } else {
        x <- accrualPlot::accrual_create_df(tmp$rando_date.date,
                                            by = factor(tmp$centre.short)
        )
      }
      return(x)
  })
  
  target <- reactive({
    if (nlevels(factor(data.randomized()$centre.short))==1)
    {
      centers_filtered <- centers %>%
      filter(centre.short %in% levels(factor(data.randomized()$centre.short)))
             
      target_vec <- centers_filtered$target
      } else {
        centers_filtered <- centers %>%
          filter(centre.short %in% names(acc()))
        target_vec <- centers_filtered$target
        names(target_vec) <- centers_filtered$centre.short
      }

    return(target_vec)
  })
  
  # target vector for tab with with different target definitions
  target_qol <- reactive({
    if (nlevels(factor(data.randomized()$centre.short))==1)
    {
      centers_filtered <- centers %>%
        filter(centre.short %in% levels(factor(data.randomized()$centre.short)))
      
      target_vec <- centers_filtered$target
    } else {
      centers_filtered <- centers %>%
        filter(centre.short %in% names(acc_qol()))
      # message(paste(" -> centers_filtered: ", centers_filtered))
      target_vec <- centers_filtered$target
      names(target_vec) <- centers_filtered$centre.short
    }
    
    return(target_vec)
  })
  
  ## Prediction plot by site
  output$predictplot_site <- renderPlotly({
    p <- accrualPlot::gg_accrual_plot_predict(acc(), target = target()) +
      theme_bw() +
      scale_x_date(labels = function(x) format(x, format = "%d %b %Y"))
    
    plotly::ggplotly(p) %>% layout(legend = list(y = 0.5))
  })
  
  ## Prediction plot with different target definitions
  output$predictplot <- renderPlotly({
    
    p <- accrualPlot::gg_accrual_plot_predict(acc_qol(), target = target_qol()) +
      theme_bw() +
      scale_x_date(labels = function(x) format(x, format = "%d %b %Y"))
    
    plotly::ggplotly(p) %>% layout(legend = list(y = 0.5))
  })
  
  
  output$recruittable <- gt::render_gt({
    gt::gt(accrualPlot::accrual_table(acc_qol(), unit = "month", format_table_date = "%b %Y")) %>%
      gt::tab_options(column_labels.hidden = TRUE)
  })
  
  }
