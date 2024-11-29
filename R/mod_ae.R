#' Adverse Events
#'
#' In development
#' @rdname mod_ae
#' @param id standard shiny id argument
#' @param label standard shiny label argument
mod_ae_ui <- function(id, label){
  #initialize namespace
  ns <- NS(id)
  
  # Generate the AE module UI
  tabItem(tabName = label,
          # A tabset panel containing a loading screen, and an authorized and unauthorized tab depending on user rights
          tabsetPanel(id = ns("switcher"), type="hidden", 
                      # Loading screen while AE server is loading
                      # Once server is loaded, it will switch to authorized or unauthorized tab depending on user rights
                      tabPanelBody("loading", icon('transfer', lib = 'glyphicon'), HTML('&nbsp;&nbsp;'), "loading... ", ),
                      
                      #Basic UI with a message when user is not authorized to access this module
                      tabPanelBody("not_authorized", icon("lock", "fa-2x"), HTML('&nbsp;&nbsp;'), "You are not authorized to access this module."), 
                      
                      # The main module UI, active once the user is authorized
                      tabPanelBody("authorized", 
                                   fluidRow(
                                     # AE filters (applicable for all tabs) are used to filter the AE based on their caracteristics
                                     # for example, you may want to filter for unexpected AE with causality certain, probable or possible (=SUSARs)
                                     box(width = 12,
                                         # Filters choices are generated on server side, based on dataset content
                                         fluidRow( 
                                           p(style="padding-left:15px", "Filter AE by characteristics: delete those that do not apply."),
                                           # Filter by report type (e.g. initial / final / follow-up)
                                           column(4, uiOutput(ns("ae_filter_report_type_ui"))), 
                                           #Filter by serverity (e.g. Mild / Moderate / Severe)
                                           column(4, uiOutput(ns("ae_filter_severity_ui"))),
                                           #Filter by expectedness (e.g. Expected / Unexpected)
                                           column(4, uiOutput(ns("ae_filter_expectedness_ui")))
                                         ),
                                         fluidRow( 
                                           column(6, uiOutput(ns("ae_filter_outcome_ui"))), 
                                           column(6, uiOutput(ns("ae_filter_causality_ui")))
                                         )
                                     ),
                                     
                                     # Tabs with plots
                                     tabBox(width = 12,
                                            title = "",
                                            id = "tabset2",
                                            
                                            tabPanel("AE occurence overtime", 
                                                     h2("Cummulative AE occurence overtime:"),
                                                     plotlyOutput(ns("ae_plot_1")), 
                                                     br(),
                                                     h2("Cummulative AE occurence overtime by center:"),
                                                     plotlyOutput(ns("ae_plot_2"))
                                            ),
                                            tabPanel("AE number by characteristics", 
                                                     h2("AE number by center and by characteristics: "), 
                                                     uiOutput(ns("ae_fact_sel_ui")),
                                                     plotlyOutput(ns("ae_plot_3")), 
                                                     br(),
                                                     h2("AE count table:"), 
                                                     uiOutput(ns("ae_table_detail_var_ui")), 
                                                     tableOutput(ns("ae_table")),
                                            ), 
                                            tabPanel(width=12, "AE list",
                                                     h2("AE list:"),
                                                     div(DT::dataTableOutput(ns("ae_table_1")), style = "font-size: 90%; width: 100%")
                                            )
                                     )
                                   )
                      )
          )
  )
}

#' @rdname mod_ae
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data data for use in calculations
mod_ae_server <- function(input, output, session, data.ae, data.ae.static, auth){
  ns <- session$ns
  
  # Get login information for the logged user from from the input parameter 'auth'
  access_granted = reactive({
    # TODO set the conditions for a user to get access to the module
    # e.g. you can use a role name or a specific variable for the module (defined in credentials.R)
    if(is.null(auth$access_ae) | is.null(auth$role)){
      return(NULL)
    }else if(auth$access_ae == TRUE | auth$role %in% c("admin", "user")){
      return(TRUE)
    }else{
      return(FALSE)
    }
  })
  
  # Shows the tabset panel with the module UI if the user is authorized to access 
  # this module, or a tabset panel with a non-authorized message if the user is not authorized. 
  observe({
    req(access_granted())
    if(access_granted())
      updateTabsetPanel(inputId = "switcher", selected = "authorized")
    else{
      updateTabsetPanel(inputId = "switcher", selected = "not_authorized")
    }
  })
  
  # TODO map the variable below to the corresponding variable names in your dataset
  record_id <- "pat_id" # unique ID of the participant
  ae_id = "ae_id" #NEW
  ae_gp_id = "ae_gp_id" #NEW
  center <- "centre.short" # center or data access group of participant
  ae_date <- "ae_date" # date of the AE
  ae_report_type <- "ae_report_type" # type of AE report (e.g. initial / follow-up / final)
  severity_level <- "severity_level" # severity level of the AE (e.g. mild / moderate / severe)
  expectedness <- "expectedness" # expectedness of the AE (e.g. expected / unexpected)
  causality <- "causality" # causality of the AE (e.g. certain / probable / possible / inlikely / not related / not assessable)
  outcome <- "outcome" # outcome of the AE (e.g. death / ongoing / resolved without sequelae / resolved with sequelae / other)
  
  # TODO list here the AE variables that are factors, they will be available in 
  # the select inputs as categories
  ae_vars <- c("severity_level", "causality", "expectedness", "outcome", "death", 
                "life_threatening", "persistant_disability", "hospitalization", 
                "congenital_anomalia_birth_defect", "ae_report_type", "centre.short") 
  
  # TODO (optional) You can specify below custom labels to be displayed in place 
  # of the default variable names from your dataset. 
  # To use default names you can uncomment the line below and comment the two following lines. 
  #var.transl <- NULL #uncomment this line to use default variables names
  var.transl <- c("pat_id" = "Patient ID", 
                  "ae_date" = "AE date", 
                  "ae_description" = "Description of AE", 
                  "severity_level" = "Severity level of AE", 
                  "causality" = "Causality / Relatedness", 
                  "expectedness" = "Expectedness",
                  "outcome" = "Outcome", 
                  "death" = "Resulted in death",
                  "life_threatening" = "Is life threatening",
                  "persistant_disability" = "Resulted in permanent disability",
                  "hospitalization" = "Required hospitalization or prolongation of hospitalization", 
                  "congenital_anomalia_birth_defect" = "Caused congenital anomaly or birth defect",
                  "ae_report_type" = "Type of AE report" , 
                  "centre.short" = "Center") #A more user friendly way to input data (?)
  var.transl <- data.frame(original = names(var.transl), new = var.transl)
  
  
  ## reactive filtered data - filtered based on AE characteristics (this variable is used in plots)
  data.ae.filtered <- reactive({
    #wait for input to be created
    req(input$ae_filter_severity, 
        input$ae_filter_outcome, 
        input$ae_filter_causality, 
        input$ae_filter_expectedness, 
        input$ae_filter_report_type, quietly = T) 
    #filter data based on AE filters in UI
    d <- data.ae() #get input data
    d <- d[d[,severity_level] %in% input$ae_filter_severity,]
    d <- d[d[,outcome] %in% input$ae_filter_outcome,]
    d <- d[d[,causality] %in% input$ae_filter_causality,]
    d <- d[d[,expectedness] %in%  input$ae_filter_expectedness,]
    d <- d[d[,ae_report_type] %in% input$ae_filter_report_type,]
    d
  })
  
  ## plot of cumulative AE number (all centers)
  output$ae_plot_1 <- renderPlotly({
    d <- data.ae.filtered() #get filtered reactive data
    d <- d[order(d[,ae_date]),] #order the AE by date
    d$cumul <- seq_along(d[,ae_date]) #get cumulative count of AE
    a <- aggregate(data = d, as.formula(paste0("cumul ~ ", ae_date)), max) #aggregate max (to acount for >1 AE on same date)
    #do the plot (+add custom label to hover text as points)
    p <- ggplot() + geom_step(aes(x=a[,ae_date], y=a$cumul, color=paste0("All (n=", max(a$cumul), ")"))) + 
      geom_point(aes(x=a[,ae_date], y=a$cumul, text=paste("AE date:", a[,ae_date], 
                                                           "<br>AE count:", a$cumul)), size=0.5, alpha=0) +
      theme_bw() + labs(x="AE date", y="AE cumulative number", color="Center") + 
      scale_color_manual(values=c("#000000"))
    #convert to plotly (+specify tooltip)
    plotly::ggplotly(p, tooltip="text")
  })
  
  ## plot of cumulative AE number (color by center)
  output$ae_plot_2 <- renderPlotly({
    d <- data.ae.filtered() #get filtered reactive data
    #order the AE by date
    d <- d[order(d[,ae_date]),] 
    
    #get cumulative AE count by center
    center_list <- sort(unique(d[,center]))
    d$cumul <- 0
    for(c in center_list){
      d$cumul[d[,center]==c] <- seq_along(d$cumul[d[,center]==c])
    }
    
    #get total AE nb by center in data frame, to add in center label on plot
    m <- aggregate(data = d, as.formula(paste0("cumul ~ ", center)), max)
    names(m)[2] <- "tot"
    d <- merge(x = d, y = m, by = center, all.x = T)
    d <- d[order(d[,ae_date]),]
    
    #do the plot (+add custom label to hover text as points)
    p <- ggplot() +  geom_step(aes(x=d[,ae_date], y=d$cumul, color=paste0(d[,center], " (n=", d$tot, ")") )) + 
      geom_point(aes(x=d[,ae_date], y=d$cumul, color=paste0(d[,center], " (n=", d$tot, ")"), 
                     text=paste('AE date: ', d[,ae_date],
                                '<br>Center:', d[,center],
                                '<br>AE count:', d$cumul)), size=0.5, alpha=0) + 
      theme_bw() + labs(x="AE date", y="AE cumulative number", color="Center")
    
    #convert to plotly (+specify tooltip)
    plotly::ggplotly(p, tooltip="text")
  })
  
  ## plot AE barplot, color by ae characteristics
  output$ae_plot_3 <- renderPlotly({
    #get input: what factor to use as color? + map to df col name
    req(input$ae_fact_sel) #wait for select input
    f <- input$ae_fact_sel
    d <- data.ae.filtered() #get filtered reactive data
    if(f != 'None'){
      if( (!f %in% names(d)) & (!is.null(var.transl)) ){
        f <- var.transl$original[var.transl$new == f] #revert back to original if label translated
      }
      a <- aggregate(data = d, as.formula(paste0(record_id, " ~ ", center, "*", f)), length)
      p <- ggplot(data=a, aes(x=get(center), y=get(record_id), fill=get(f))) + 
        geom_col(aes(text=paste0("Center: ", get(center), 
                                 "<br>", input$ae_fact_sel, ": ", get(f),
                                 "<br>AE count: ", get(record_id)))) + 
        theme_bw() + labs(x = "Center", y="AE count", fill=input$ae_fact_sel) + 
        geom_text(aes(x=get(center), y=get(record_id), label = paste0("n=", get(record_id)), group=get(f)), 
                  position = position_stack(vjust = 0.5), color="lightgray", size=3)
      plotly::ggplotly(p, tooltip="text")
    }else{
      a <- aggregate(data = d, as.formula(paste0(record_id, " ~ ", center)), length)
      p <- ggplot(data=a, aes(x=get(center), y=get(record_id))) + 
        geom_col(aes(text=paste("Center:", get(center), 
                                "<br>AE count:", get(record_id)))) + 
        theme_bw() + labs(x = "Center", y="AE count") + 
        geom_text(aes(label = paste0("n=", get(record_id)), y = get(record_id) + 0.5), 
                  color="darkgray", size=3)
      plotly::ggplotly(p, tooltip="text")
    }
  })
  
  # Data Table of AE count aggregate
  output$ae_table <- renderTable({
    req(input$ae_table_detail_var) #wait for select input
    d <- data.ae.filtered()
    if(input$ae_table_detail_var != "None"){
      f <- input$ae_table_detail_var
      if( (!f %in% names(d)) & (!is.null(var.transl)) ){
        f <- var.transl$original[var.transl$new == f] #revert back to original if label translated
      }
      
      ae_table <- aggregate(data = d, as.formula(paste0(record_id, "~", f, "*", center)), length)
      ae_table <- reshape2::dcast(ae_table, formula = as.formula(paste0(center, " ~ ", f)))
      if( length(unique(ae_table[,center])) > 1 ){
        ae_sum <- apply(as.data.frame(ae_table[,2:ncol(ae_table)]), 2, function(x){sum(x, na.rm = T)})
        ae_table <- rbind(c("All", ae_sum), ae_table)
      }
      ae_table <- ae_table[, c(1, ncol(ae_table):2)]
      ae_table[is.na(ae_table)] <- 0 #replaced NA by 0
      names(ae_table)[1] <- "Center"
      if(ncol(ae_table) > 2){
        ae_table$Total <- apply(ae_table[,2:ncol(ae_table)], 1, function(x){as.character(sum(as.numeric(x), na.rm=T))})
      }
    }else{
      ae_table <- aggregate(data=d, as.formula(paste0(record_id, " ~ ", center)), length)
      names(ae_table) <- c("Center", "Total")
      if( length(unique(ae_table$Center)) > 1 ){
        ae_table <- rbind( c("All", as.character(sum(as.numeric(ae_table$Total)))), ae_table )
      }
    }
    ae_table
  })
  
  ## DataTable of all AE
  output$ae_table_1 <- DT::renderDataTable({
    d <- data.ae.filtered()[order(data.ae.filtered()[,ae_date]),]
    col_names <- names(d)
    for (i in 1:length(col_names)){
      if(col_names[i] %in% var.transl$original){
        col_names[i] <- var.transl$new[var.transl$original == col_names[i]]
      }
    }
    DT::datatable(d, rownames = FALSE, colnames = col_names, class = 'cell-border stripe', filter = "top", 
                  options = list(scrollX = T, pageLength = 10, autoWidth = TRUE, 
                                 columnDefs = list(list(width = "140px", targets = c(9)), 
                                                   list(width = "100px", targets = c(10)))))
  })
  
  
  ## Dynamic UI code
  #list of content - report type
  ae_report_type_list <- reactive({
    unique(data.ae.static[, ae_report_type])
  })
  #list of content - AE severity level
  severity_level_list <- reactive({
    unique(data.ae.static[, severity_level])
  })
  #list of content - AE expectedness
  ae_expectedness_list <- reactive({
    unique(data.ae.static[, expectedness])
  })
  #list of content - AE causality
  ae_causality_list <- reactive({
    unique(data.ae.static[, causality])
  })
  #list of content - AE outcome
  ae_outcome_list <- reactive({
    unique(data.ae.static[, outcome])
  })
  #list of content - AE variables
  ae_var_list <- reactive({
    nm <- names(data.ae.static)[names(data.ae.static) %in% ae_vars]
    #translate varnames if not null
    if(!is.null(var.transl)){
      for(i in 1:nrow(var.transl)){
        if(var.transl$original[i] %in% nm){
          nm[nm == var.transl$original[i]] <- var.transl$new[i]
        }
      }
    }
    nm
  })
  
  # Add an * to filters when they are active
  observe({
    if(length(input$ae_filter_report_type) == length(ae_report_type_list()) ){
      l <- "Filter by report type"
    }else{
      l <- "Filter by report type*"
    }
    updateSelectInput(session, 'ae_filter_report_type', label = l, 
                      choices = NULL, selected = NULL)
  })
  observe({
    if(length(input$ae_filter_severity) == length(severity_level_list())){
      l <- "Filter by severity"
    }else{
      l <- "Filter by severity*"
    }
    updateSelectInput(session, 'ae_filter_severity', label = l,  
                      choices = NULL, selected = NULL)
  })
  observe({
    if(length(input$ae_filter_expectedness) == length(ae_expectedness_list())){
      l <- "Filter by expectedness"
    }else{
      l <- "Filter by expectedness*"
    }
    updateSelectInput(session, 'ae_filter_expectedness', label = l,  
                      choices = NULL, selected = NULL)
  })
  observe({
    if(length(input$ae_filter_causality) == length(ae_causality_list())){
      l <- "Filter by causality"
    }else{
      l <- "Filter by causality*"
    }
    updateSelectInput(session, 'ae_filter_causality', label = l,  
                      choices = NULL, selected = NULL)
  })
  observe({
    if(length(input$ae_filter_outcome) == length(ae_outcome_list())){
      l <- "Filter by outcome"
    }else{
      l <- "Filter by outcome*"
    }
    updateSelectInput(session, 'ae_filter_outcome', label = l,  
                      choices = NULL, selected = NULL)
  })
  
  # Dynamic UI - report type
  output$ae_filter_report_type_ui <- renderUI({
    choices <- ae_report_type_list()
    selectInput(inputId = ns('ae_filter_report_type'),
                label = "Filter by report type", 
                choices = choices, multiple = T, 
                selected = choices)
  })
  # Dynamic UI - AE severity
  output$ae_filter_severity_ui <- renderUI({
    choices <- severity_level_list()
    selectInput(ns("ae_filter_severity"), 
                label = "Filter by severity", 
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - AE expectedness
  output$ae_filter_expectedness_ui <- renderUI({
    choices <- ae_expectedness_list()
    selectInput(ns("ae_filter_expectedness"), 
                label = "Filter by expectedness",  
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - AE causality
  output$ae_filter_causality_ui <- renderUI({
    choices <- ae_causality_list()
    selectInput(ns("ae_filter_causality"), 
                label = "Filter by causality",  
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - AE outcome
  output$ae_filter_outcome_ui <- renderUI({
    choices <- ae_outcome_list()
    selectInput(ns("ae_filter_outcome"), 
                label = "Filter by outcome",  
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - AE variables
  output$ae_table_detail_var_ui <- renderUI({
    choices <- c("None", ae_var_list())
    selectInput(ns("ae_table_detail_var"), 
                label = "Detail by AE characteristics",  
                choices = choices, multiple = F, 
                selected = "None") 
  })
  output$ae_fact_sel_ui <- renderUI({
    choices <- c("None", ae_var_list())
    selectInput(ns("ae_fact_sel"), 
                label = "Color by AE characteristics:",  
                choices = choices, multiple = F, 
                selected = "None") 
  })
}