#' Serious Adverse Events
#'
#' In development 
#' This module is intended to get an overview of of the safety data. 
#' 
#' 
#' @author Laurent Brodier (Laurent.Brodier@hug.ch)
#' @import ggplot2
#' @import plotly
#' @import reshape2
#' @import DT
#' 
#' @rdname mod_sae
#' @param id standard shiny id argument
#' @param label standard shiny label argument

mod_sae_st_ui <- function(id, label){
  #initialize namespace
  ns <- NS(id)
  
  # Generate the SAE module UI
  tabItem(tabName = label,
          # A tabset panel containing a loading screen, and an authorized and unauthorized tab depending on user rights
          tabsetPanel(id = ns("switcher"), type="hidden", 
                      # Loading screen while SAE server is loading
                      # Once server is loaded, it will switch to authorized or unauthorized tab depending on user rights
                      tabPanelBody("loading", icon('transfer', lib = 'glyphicon'), HTML('&nbsp;&nbsp;'), "loading... ", ),
                      
                      #Basic UI with a message when user is not authorized to access this module
                      tabPanelBody("not_authorized", icon("lock", "fa-2x"), HTML('&nbsp;&nbsp;'), "You are not authorized to access this module."), 
                      
                      # The main module UI, active once the user is authorized
                      tabPanelBody("authorized", 
                                   fluidRow(
                                     # SAE filters (applicable for all tabs) are used to filter the SAE based on their caracteristics
                                     # for example, you may want to filter for unexpected SAE with causality certain, probable or possible (=SUSARs)
                                     box(width = 12,
                                         # Filters choices are generated on server side, based on dataset content
                                         fluidRow( 
                                           p(style="padding-left:15px", "Filter SAE by characteristics: delete those that do not apply."),
                                           # Filter by report type (e.g. initial / final / follow-up)
                                           column(4, uiOutput(ns("sae_filter_report_type_ui"))), 
                                           #Filter by serverity (e.g. Mild / Moderate / Severe)
                                           column(4, uiOutput(ns("sae_filter_severity_ui"))),
                                           #Filter by expectedness (e.g. Expected / Unexpected)
                                           column(4, uiOutput(ns("sae_filter_expectedness_ui")))
                                         ),
                                         fluidRow( 
                                           column(6, uiOutput(ns("sae_filter_outcome_ui"))), 
                                           column(6, uiOutput(ns("sae_filter_causality_ui")))
                                         )
                                     ),
                                     
                                     # Tabs with plots
                                     tabBox(width = 12,
                                            title = "",
                                            id = "tabset2",
                                            
                                            tabPanel("SAE occurence overtime", 
                                                     h2("Cummulative SAE occurence overtime:"),
                                                     plotlyOutput(ns("sae_plot_1")), 
                                                     br(),
                                                     h2("Cummulative SAE occurence overtime by center:"),
                                                     plotlyOutput(ns("sae_plot_2"))
                                            ),
                                            tabPanel("SAE number by characteristics", 
                                                     h2("SAE number by center and by characteristics: "), 
                                                     uiOutput(ns("sae_fact_sel_ui")),
                                                     plotlyOutput(ns("sae_plot_3")), 
                                                     br(),
                                                     h2("SAE count table:"), 
                                                     uiOutput(ns("sae_table_detail_var_ui")), 
                                                     tableOutput(ns("sae_table")),
                                            ), 
                                            tabPanel(width=12, "SAE list",
                                                     h2("SAE list:"),
                                                     div(DT::dataTableOutput(ns("sae_table_1")), style = "font-size: 90%; width: 100%")
                                            ),
                                            tabPanel(width=12, "AE/SAE distribution",
                                                     p("in devel... first preview with non reactive sT data"),
                                                     fluidRow(
                                                       column(12, h3("Histogram of the number of events (AE only or AE+SAE) by patient"), plotlyOutput(ns("sae_histogram_1")))
                                                     ),
                                                     fluidRow(
                                                       column(6, h3("Histogram of the nb of AE (AE only)"), plotlyOutput(ns("sae_histogram_2"))),
                                                       column(6, h3("Histogram of the nb of SAE (AE+SAE)"), plotlyOutput(ns("sae_histogram_3")))
                                                     ),
                                                     fluidRow(
                                                       column(6, h3("Number of events by patient (violin)"), plotlyOutput(ns("sae_violin_1"))),
                                                       column(6, h3("Number of events by patient (boxplot)"), plotlyOutput(ns("sae_boxplot_1")))
                                                     ),
                                                     fluidRow(
                                                       column(12, h3("Table of the number of events by patient"), tableOutput(ns("sae_nb_table")))
                                                     )
                                            ),
                                            tabPanel(width=12, "AE/SAE follow-up",
                                                     p("in devel... first preview with non reactive sT data"),
                                                     fluidRow(
                                                       column(12, h3("Histogram of the number of FU (AE+SAE FU) by event"), plotlyOutput(ns("sae_fu_histogram_1")))
                                                     ),
                                                     fluidRow(
                                                       column(6, h3("Histogram of the nb of AE FU by event"), plotlyOutput(ns("sae_fu_histogram_2"))),
                                                       column(6, h3("Histogram of the nb of SAE FU by event:"), plotlyOutput(ns("sae_fu_histogram_3")))
                                                     ),
                                                     fluidRow(
                                                       column(6, h3("SAE follow-up (violin)"), plotlyOutput(ns("sae_fu_violin_2"))),
                                                       column(6, h3("SAE follow-up (boxplot)"), plotlyOutput(ns("sae_fu_boxplot_2")))
                                                     ), 
                                                     fluidRow(
                                                       column(12, h3("Table of the number SAE follow-up"), tableOutput(ns("sae_fu_table")))
                                                     )
                                            )
                                     )
                                   )
                      )
          )
  )
}

#' @rdname mod_sae
#' @param input standard shiny input argument
#' @param output standard shiny output argument
#' @param session standard shiny session argument
#' @param data.sae data for use in calculations
mod_sae_st_server <- function(input, output, session, data.sae, data.sae.static, ae, sae, auth){
  ns <- session$ns
  
  # Get login information for the logged user from from the input parameter 'auth'
  access_granted = reactive({
    # TODO set the conditions for a user to get access to the module
    # e.g. you can use a role name or a specific variable for the module (defined in credentials.R)
    if(is.null(auth$access_sae) | is.null(auth$role)){
      return(NULL)
    }else if(auth$access_sae == TRUE | auth$role %in% c("admin", "user")){
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
  sae_id = "sae_id" #NEW
  sae_gp_id = "sae_gp_id" #NEW
  center <- "centre.short" # center or data access group of participant
  sae_date <- "sae_date" # date of the SAE
  sae_report_type <- "sae_report_type" # type of SAE report (e.g. initial / follow-up / final)
  severity_level <- "severity_level" # severity level of the SAE (e.g. mild / moderate / severe)
  expectedness <- "expectedness" # expectedness of the SAE (e.g. expected / unexpected)
  causality <- "causality" # causality of the SAE (e.g. certain / probable / possible / inlikely / not related / not assessable)
  outcome <- "outcome" # outcome of the SAE (e.g. death / ongoing / resolved without sequelae / resolved with sequelae / other)
  
  # TODO list here the SAE variables that are factors, they will be available in 
  # the select inputs as categories
  sae_vars <- c("severity_level", "causality", "expectedness", "outcome", "death", 
                "life_threatening", "persistant_disability", "hospitalization", 
                "congenital_anomalia_birth_defect", "sae_report_type", "centre.short") 
  
  # TODO (optional) You can specify below custom labels to be displayed in place 
  # of the default variable names from your dataset. 
  # To use default names you can uncomment the line below and comment the two following lines. 
  #var.transl <- NULL #uncomment this line to use default variables names
  var.transl <- c("pat_id" = "Patient ID", 
                  "sae_date" = "SAE date", 
                  "sae_description" = "Description of SAE", 
                  "severity_level" = "Severity level of SAE", 
                  "causality" = "Causality / Relatedness", 
                  "expectedness" = "Expectedness",
                  "outcome" = "Outcome", 
                  "death" = "Resulted in death",
                  "life_threatening" = "Is life threatening",
                  "persistant_disability" = "Resulted in permanent disability",
                  "hospitalization" = "Required hospitalization or prolongation of hospitalization", 
                  "congenital_anomalia_birth_defect" = "Caused congenital anomaly or birth defect",
                  "sae_report_type" = "Type of SAE report" , 
                  "centre.short" = "Center") #A more user friendly way to input data (?)
  var.transl <- data.frame(original = names(var.transl), new = var.transl)
  
  
  ## reactive filtered data - filtered based on SAE characteristics (this variable is used in plots)
  data.sae.filtered <- reactive({
    #wait for input to be created
    req(input$sae_filter_severity, 
        input$sae_filter_outcome, 
        input$sae_filter_causality, 
        input$sae_filter_expectedness, 
        input$sae_filter_report_type, quietly = T) 
    #filter data based on SAE filters in UI
    d <- data.sae() #get input data
    d <- d[d[,severity_level] %in% input$sae_filter_severity,]
    d <- d[d[,outcome] %in% input$sae_filter_outcome,]
    d <- d[d[,causality] %in% input$sae_filter_causality,]
    d <- d[d[,expectedness] %in%  input$sae_filter_expectedness,]
    d <- d[d[,sae_report_type] %in% input$sae_filter_report_type,]
    d
  })
  
  ## plot of cumulative SAE number (all centers)
  output$sae_plot_1 <- renderPlotly({
    d <- data.sae.filtered() #get filtered reactive data
    d <- d[order(d[,sae_date]),] #order the SAE by date
    d$cumul <- seq_along(d[,sae_date]) #get cumulative count of SAE
    a <- aggregate(data = d, as.formula(paste0("cumul ~ ", sae_date)), max) #aggregate max (to acount for >1 SAE on same date)
    #do the plot (+add custom label to hover text as points)
    p <- ggplot() + geom_step(aes(x=a[,sae_date], y=a$cumul, color=paste0("All (n=", max(a$cumul), ")"))) + 
      geom_point(aes(x=a[,sae_date], y=a$cumul, text=paste("SAE date:", a[,sae_date], 
                                                           "<br>SAE count:", a$cumul)), size=0.5, alpha=0) +
      theme_bw() + labs(x="SAE date", y="SAE cumulative number", color="Center") + 
      scale_color_manual(values=c("#000000"))
    #convert to plotly (+specify tooltip)
    plotly::ggplotly(p, tooltip="text")
  })
  
  ## plot of cumulative SAE number (color by center)
  output$sae_plot_2 <- renderPlotly({
    d <- data.sae.filtered() #get filtered reactive data
    #order the SAE by date
    d <- d[order(d[,sae_date]),] 
    
    #get cumulative SAE count by center
    center_list <- sort(unique(d[,center]))
    d$cumul <- 0
    for(c in center_list){
      d$cumul[d[,center]==c] <- seq_along(d$cumul[d[,center]==c])
    }
    
    #get total SAE nb by center in data frame, to add in center label on plot
    m <- aggregate(data = d, as.formula(paste0("cumul ~ ", center)), max)
    names(m)[2] <- "tot"
    d <- merge(x = d, y = m, by = center, all.x = T)
    d <- d[order(d[,sae_date]),]
    
    #do the plot (+add custom label to hover text as points)
    p <- ggplot() +  geom_step(aes(x=d[,sae_date], y=d$cumul, color=paste0(d[,center], " (n=", d$tot, ")") )) + 
      geom_point(aes(x=d[,sae_date], y=d$cumul, color=paste0(d[,center], " (n=", d$tot, ")"), 
                     text=paste('SAE date: ', d[,sae_date],
                                '<br>Center:', d[,center],
                                '<br>SAE count:', d$cumul)), size=0.5, alpha=0) + 
      theme_bw() + labs(x="SAE date", y="SAE cumulative number", color="Center")
    
    #convert to plotly (+specify tooltip)
    plotly::ggplotly(p, tooltip="text")
  })
  
  ## plot SAE barplot, color by sae characteristics
  output$sae_plot_3 <- renderPlotly({
    #get input: what factor to use as color? + map to df col name
    req(input$sae_fact_sel) #wait for select input
    f <- input$sae_fact_sel
    d <- data.sae.filtered() #get filtered reactive data
    if(f != 'None'){
      if( (!f %in% names(d)) & (!is.null(var.transl)) ){
        f <- var.transl$original[var.transl$new == f] #revert back to original if label translated
      }
      a <- aggregate(data = d, as.formula(paste0(record_id, " ~ ", center, "*", f)), length)
      p <- ggplot(data=a, aes(x=get(center), y=get(record_id), fill=get(f))) + 
        geom_col(aes(text=paste0("Center: ", get(center), 
                                 "<br>", input$sae_fact_sel, ": ", get(f),
                                 "<br>SAE count: ", get(record_id)))) + 
        theme_bw() + labs(x = "Center", y="SAE count", fill=input$sae_fact_sel) + 
        geom_text(aes(x=get(center), y=get(record_id), label = paste0("n=", get(record_id)), group=get(f)), 
                  position = position_stack(vjust = 0.5), color="lightgray", size=3)
      plotly::ggplotly(p, tooltip="text")
    }else{
      a <- aggregate(data = d, as.formula(paste0(record_id, " ~ ", center)), length)
      p <- ggplot(data=a, aes(x=get(center), y=get(record_id))) + 
        geom_col(aes(text=paste("Center:", get(center), 
                                "<br>SAE count:", get(record_id)))) + 
        theme_bw() + labs(x = "Center", y="SAE count") + 
        geom_text(aes(label = paste0("n=", get(record_id)), y = get(record_id) + 0.5), 
                  color="darkgray", size=3)
      plotly::ggplotly(p, tooltip="text")
    }
  })
  
  # Data Table of SAE count aggregate
  output$sae_table <- renderTable({
    req(input$sae_table_detail_var) #wait for select input
    d <- data.sae.filtered()
    if(input$sae_table_detail_var != "None"){
      f <- input$sae_table_detail_var
      if( (!f %in% names(d)) & (!is.null(var.transl)) ){
        f <- var.transl$original[var.transl$new == f] #revert back to original if label translated
      }
      
      sae_table <- aggregate(data = d, as.formula(paste0(record_id, "~", f, "*", center)), length)
      sae_table <- reshape2::dcast(sae_table, formula = as.formula(paste0(center, " ~ ", f)))
      if( length(unique(sae_table[,center])) > 1 ){
        sae_sum <- apply(as.data.frame(sae_table[,2:ncol(sae_table)]), 2, function(x){sum(x, na.rm = T)})
        sae_table <- rbind(c("All", sae_sum), sae_table)
      }
      sae_table <- sae_table[, c(1, ncol(sae_table):2)]
      sae_table[is.na(sae_table)] <- 0 #replaced NA by 0
      names(sae_table)[1] <- "Center"
      if(ncol(sae_table) > 2){
        sae_table$Total <- apply(sae_table[,2:ncol(sae_table)], 1, function(x){as.character(sum(as.numeric(x), na.rm=T))})
      }
    }else{
      sae_table <- aggregate(data=d, as.formula(paste0(record_id, " ~ ", center)), length)
      names(sae_table) <- c("Center", "Total")
      if( length(unique(sae_table$Center)) > 1 ){
        sae_table <- rbind( c("All", as.character(sum(as.numeric(sae_table$Total)))), sae_table )
      }
    }
    sae_table
  })
  
  ## DataTable of all SAE
  output$sae_table_1 <- DT::renderDataTable({
    d <- data.sae.filtered()[order(data.sae.filtered()[,sae_date]),]
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
  sae_report_type_list <- reactive({
    unique(data.sae.static[, sae_report_type])
  })
  #list of content - SAE severity level
  severity_level_list <- reactive({
    unique(data.sae.static[, severity_level])
  })
  #list of content - SAE expectedness
  sae_expectedness_list <- reactive({
    unique(data.sae.static[, expectedness])
  })
  #list of content - SAE causality
  sae_causality_list <- reactive({
    unique(data.sae.static[, causality])
  })
  #list of content - SAE outcome
  sae_outcome_list <- reactive({
    unique(data.sae.static[, outcome])
  })
  #list of content - SAE variables
  sae_var_list <- reactive({
    nm <- names(data.sae.static)[names(data.sae.static) %in% sae_vars]
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
    if(length(input$sae_filter_report_type) == length(sae_report_type_list()) ){
      l <- "Filter by report type"
    }else{
      l <- "Filter by report type*"
    }
    updateSelectInput(session, 'sae_filter_report_type', label = l, 
                      choices = NULL, selected = NULL)
  })
  observe({
    if(length(input$sae_filter_severity) == length(severity_level_list())){
      l <- "Filter by severity"
    }else{
      l <- "Filter by severity*"
    }
    updateSelectInput(session, 'sae_filter_severity', label = l,  
                      choices = NULL, selected = NULL)
  })
  observe({
    if(length(input$sae_filter_expectedness) == length(sae_expectedness_list())){
      l <- "Filter by expectedness"
    }else{
      l <- "Filter by expectedness*"
    }
    updateSelectInput(session, 'sae_filter_expectedness', label = l,  
                      choices = NULL, selected = NULL)
  })
  observe({
    if(length(input$sae_filter_causality) == length(sae_causality_list())){
      l <- "Filter by causality"
    }else{
      l <- "Filter by causality*"
    }
    updateSelectInput(session, 'sae_filter_causality', label = l,  
                      choices = NULL, selected = NULL)
  })
  observe({
    if(length(input$sae_filter_outcome) == length(sae_outcome_list())){
      l <- "Filter by outcome"
    }else{
      l <- "Filter by outcome*"
    }
    updateSelectInput(session, 'sae_filter_outcome', label = l,  
                      choices = NULL, selected = NULL)
  })
  
  # Dynamic UI - report type
  output$sae_filter_report_type_ui <- renderUI({
    choices <- sae_report_type_list()
    selectInput(inputId = ns('sae_filter_report_type'),
                label = "Filter by report type", 
                choices = choices, multiple = T, 
                selected = choices)
  })
  # Dynamic UI - SAE severity
  output$sae_filter_severity_ui <- renderUI({
    choices <- severity_level_list()
    selectInput(ns("sae_filter_severity"), 
                label = "Filter by severity", 
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - SAE expectedness
  output$sae_filter_expectedness_ui <- renderUI({
    choices <- sae_expectedness_list()
    selectInput(ns("sae_filter_expectedness"), 
                label = "Filter by expectedness",  
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - SAE causality
  output$sae_filter_causality_ui <- renderUI({
    choices <- sae_causality_list()
    selectInput(ns("sae_filter_causality"), 
                label = "Filter by causality",  
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - SAE outcome
  output$sae_filter_outcome_ui <- renderUI({
    choices <- sae_outcome_list()
    selectInput(ns("sae_filter_outcome"), 
                label = "Filter by outcome",  
                choices = choices, multiple = T, 
                selected = choices) 
  })
  # Dynamic UI - SAE variables
  output$sae_table_detail_var_ui <- renderUI({
    choices <- c("None", sae_var_list())
    selectInput(ns("sae_table_detail_var"), 
                label = "Detail by SAE characteristics",  
                choices = choices, multiple = F, 
                selected = "None") 
  })
  output$sae_fact_sel_ui <- renderUI({
    choices <- c("None", sae_var_list())
    selectInput(ns("sae_fact_sel"), 
                label = "Color by SAE characteristics:",  
                choices = choices, multiple = F, 
                selected = "None") 
  })
  
  #### new tabs sT 2023 ####
  # TODO # replace later with data passed to module
  #ae_path_st <- "s_export_CSV_DEVL8_20230320-145336/ae.csv"
  #sae_path_st <- "s_export_CSV_DEVL8_20230320-145336/sae.csv"
  color_aesae <- 'royalblue'
  color_ae <- 'mediumseagreen'
  color_sae <- 'tomato'
  
  #nb event by patient - require AE + SAE datasets from sT
  nb_event_by_patient <- reactive({
    # read export from sT for AE and SAE
    # TODO # replace later with data passed to module
    #ae <- read.delim(ae_path_st, header = T, sep = ",")
    #sae <- read.delim(sae_path_st, header = T, sep = ",")
    
    # get nb of events + nb of AE + nb of SAE by patient
    nb_event_by_patient_ae <- aggregate(data = ae, ae$mnpaeid ~ mnpaid, FUN = function(x){length(unique(x))})
    nb_event_by_patient_sae <- aggregate(data = sae, sae$mnpaeid ~ mnpaid, FUN = function(x){length(unique(x))})               
    nb_event_by_patient <- merge(nb_event_by_patient_ae, nb_event_by_patient_sae, by = "mnpaid", all = T)
    names(nb_event_by_patient) <- c("mnpaid", "nb.event", "nb.sae")
    nb_event_by_patient$nb.ae = nb_event_by_patient$nb.event - nb_event_by_patient$nb.sae
    rm(nb_event_by_patient_ae, nb_event_by_patient_sae)
    
    # TODO # get a patient list somehow and add 0 for all not in the list
    patients_list <- c("TEST-0001", "TEST-0002", "TEST-0003", "TEST-0004", "TEST-0005", 
                       "TEST-0006", "TEST-0007", "TEST-0008")
    tmp <- data.frame(mnpaid = patients_list[!patients_list %in% nb_event_by_patient$mnpaid], 
                      nb.event = 0, nb.sae = 0, nb.ae=0)
    # merge and reorder columns
    nb_event_by_patient <- rbind(nb_event_by_patient, tmp)
    nb_event_by_patient <- nb_event_by_patient[,c(1:2, 4, 3)]
    nb_event_by_patient$nb.event <- as.integer(nb_event_by_patient$nb.event)
    nb_event_by_patient$nb.ae <- as.integer(nb_event_by_patient$nb.ae)
    nb_event_by_patient$nb.sae <- as.integer(nb_event_by_patient$nb.sae)
    rm(tmp)
    return(nb_event_by_patient)
  })
  nb_event_by_patient_melt <- reactive({
    # prepare data for violin plots of nb events
    m <- melt(nb_event_by_patient(), id.vars = "mnpaid")
    names(m) <- c("mnpaid", "aesae", "count")
    # set readable labels for plots
    m$label <- factor(sapply(m$aesae, function(x){if(x=="nb.event"){"AE + SAE"}else if(x=="nb.ae"){"AE"}else if(x=="nb.sae"){"SAE"}else{x}}), levels = c("AE + SAE", "AE", "SAE"))
    return(m)
  })
  
  #nb fu by patient and by event (mnpaeid)
  nb_fu_by_aeid <- reactive({
    # read export from sT for AE and SAE
    # TODO # replace later with data passed to module
    #ae <- read.delim(ae_path_st, header = T, sep = ",")
    #sae <- read.delim(sae_path_st, header = T, sep = ",")
    
    nb_fu_by_aeid_ae <- aggregate(data=ae, mnpaefuid ~ mnpaeid * mnpaid, length)
    nb_fu_by_aeid_sae <- aggregate(data=sae, mnpaefuid ~ mnpaeid, length)
    nb_fu_by_aeid <- merge(nb_fu_by_aeid_ae, nb_fu_by_aeid_sae, by = "mnpaeid", suffixes = c(".event", ".sae"), all = T)
    nb_fu_by_aeid[is.na(nb_fu_by_aeid)] <- 0
    nb_fu_by_aeid$mnpaefuid.ae <- nb_fu_by_aeid$mnpaefuid.event - nb_fu_by_aeid$mnpaefuid.sae
    rm(nb_fu_by_aeid_ae, nb_fu_by_aeid_sae)
    nb_fu_by_aeid <- nb_fu_by_aeid[, c(1:3, 5, 4)]
    names(nb_fu_by_aeid)[3:5] <- c("nb.event", "nb.ae", "nb.sae")
    nb_fu_by_aeid$nb.ae <- as.integer(nb_fu_by_aeid$nb.ae)
    nb_fu_by_aeid$nb.sae <- as.integer(nb_fu_by_aeid$nb.sae)
    return(nb_fu_by_aeid)
  })
  nb_fu_by_aeid_melt <- reactive({
    m_nbfu <- melt(nb_fu_by_aeid(), id.vars = c("mnpaeid", "mnpaid"))
    names(m_nbfu)[3:4] <- c("aesae", "count")
    m_nbfu$label <- factor(sapply(m_nbfu$aesae, function(x){if(x=="nb.event"){"AE + SAE"}else if(x=="nb.ae"){"AE"}else if(x=="nb.sae"){"SAE"}else{x}}), levels = c("AE + SAE", "AE", "SAE"))
    return(m_nbfu)
  })
  
  #plots histogram distribution
  output$sae_histogram_1 <- renderPlotly({
    g <- ggplot(data=nb_event_by_patient_melt()[nb_event_by_patient_melt()$aesae == 'nb.event',]) + 
      geom_histogram(aes(x=count), fill=color_aesae, binwidth = 0.5) +
      theme_bw() + labs(x="nb of events (AE+SAE)", y="nb of patients") + 
      scale_y_continuous(breaks = seq(0, sum(nb_event_by_patient_melt()$count), by = 1), minor_breaks = 0) +
      scale_x_continuous(breaks = seq(0, sum(nb_event_by_patient_melt()$count), by = 1), minor_breaks = 0)
    ggplotly(g, tooltip = NULL)
  })
  output$sae_histogram_2 <- renderPlotly({
    g <- ggplot(data=nb_event_by_patient_melt()[nb_event_by_patient_melt()$aesae == 'nb.ae',]) + 
      geom_histogram(aes(x=count), fill=color_ae, binwidth = 0.5) +
      theme_bw() + labs(x="nb of AE", y="nb of patients") + 
      scale_y_continuous(breaks = seq(0, sum(nb_event_by_patient_melt()$count), by = 1), minor_breaks = 0) +
      scale_x_continuous(breaks = seq(0, sum(nb_event_by_patient_melt()$count), by = 1), minor_breaks = 0)
    ggplotly(g, tooltip = NULL)
  })
  output$sae_histogram_3 <- renderPlotly({
    g <- ggplot(data=nb_event_by_patient_melt()[nb_event_by_patient_melt()$aesae == 'nb.sae',]) + 
      geom_histogram(aes(x=count), fill=color_sae, binwidth = 0.5) +
      theme_bw() + labs(x="nb of SAE", y="nb of patients") + 
      scale_y_continuous(breaks = seq(0, sum(nb_event_by_patient_melt()$count), by = 1), minor_breaks = 0) +
      scale_x_continuous(breaks = seq(0, sum(nb_event_by_patient_melt()$count), by = 1), minor_breaks = 0)
    ggplotly(g, tooltip = NULL)
  })
  #plots violin + boxplot distribution
  output$sae_violin_1 <- renderPlotly({
    g <- ggplot(data=nb_event_by_patient_melt(), aes(x=label, y=count, color=label, fill=label)) + 
      geom_violin(alpha=0.1) + 
      geom_jitter(aes(text=paste0(mnpaid, "<br>", label, ": ", count)), 
                  height = 0.05, width=0.2, alpha=0.5, size=1.5) + 
      scale_color_manual(name="event type", values=c("AE + SAE"=color_aesae, "AE"=color_ae, "SAE"=color_sae)) +
      scale_fill_manual(name="event type", values=c("AE + SAE"=color_aesae, "AE"=color_ae, "SAE"=color_sae)) +
      theme_bw() + labs(x="event type", y="count")
    
    p <- ggplotly(g, tooltip = "text")
    # dirty way to remove legend/hover on ggplotly object... 
    for (i in 1:length(p$x$data)){
      if(p$x$data[[i]]$mode == "markers"){
        # remove legend for points (only if color/fill not in inherited aesthetics)
        p$x$data[[i]]$text <- c(p$x$data[[i]]$text, "") 
        p$x$data[[i]]$showlegend <- FALSE
      }else if(p$x$data[[i]]$mode == "lines"){
        p$x$data[[i]]$hoverinfo = "none" # remove density hover
      }
    }
    return(p)
  })
  output$sae_boxplot_1 <- renderPlotly({
    g <- ggplot(data=nb_event_by_patient_melt(), aes(x=label, y=count, color=label, fill=label)) + 
      geom_boxplot(color="gray", fill=NA, alpha=0.1) + 
      geom_jitter(aes(text=paste0(mnpaid, "<br>", label, ": ", count)), 
                  height = 0.05, width=0.2, alpha=0.5, size=1.5) + 
      scale_color_manual(name="event type", values=c("AE + SAE"=color_aesae, "AE"=color_ae, "SAE"=color_sae)) +
      scale_fill_manual(name="event type", values=c("AE + SAE"=color_aesae, "AE"=color_ae, "SAE"=color_sae)) +
      theme_bw() + labs(x="event type", y="count")
    
    p <- ggplotly(g, tooltip = "text")
    # dirty way to remove legend/hover on ggplotly object... 
    for (i in 1:length(p$x$data)){
      if(p$x$data[[i]]$type == "box"){
        #p$x$data[[i]]$hoverinfo = "none" # remove boxplot hover
        #p$x$data[[i]]$showlegend <- FALSE #remove legend for boxplot
      }
    }
    return(p)
  })
  output$sae_nb_table <- renderTable({
    return(nb_event_by_patient())
  })
  #plots histogram FU
  output$sae_fu_histogram_1 <- renderPlotly({
    g <- ggplot(data=nb_fu_by_aeid_melt()[nb_fu_by_aeid_melt()$aesae == 'nb.event',]) + 
      geom_histogram(aes(x=count), fill=color_aesae, binwidth = 0.5) +
      theme_bw() + labs(x="nb of AE+SAE follow-ups", y="nb of events") + 
      scale_y_continuous(breaks = seq(0, sum(nb_fu_by_aeid_melt()$count), by = 1), minor_breaks = 0) +
      scale_x_continuous(breaks = seq(0, sum(nb_fu_by_aeid_melt()$count), by = 1), minor_breaks = 0)
    ggplotly(g, tooltip = NULL)
  })
  output$sae_fu_histogram_2 <- renderPlotly({
    g <- ggplot(data=nb_fu_by_aeid_melt()[nb_fu_by_aeid_melt()$aesae == 'nb.ae',]) + 
      geom_histogram(aes(x=count), fill=color_ae, binwidth = 0.5) +
      theme_bw() + labs(x="nb of AE follow-ups", y="nb of events") + 
      scale_y_continuous(breaks = seq(0, sum(nb_fu_by_aeid_melt()$count), by = 1), minor_breaks = 0) +
      scale_x_continuous(breaks = seq(0, sum(nb_fu_by_aeid_melt()$count), by = 1), minor_breaks = 0)
    ggplotly(g, tooltip = NULL)
  })
  output$sae_fu_histogram_3 <- renderPlotly({
    g <- ggplot(data=nb_fu_by_aeid_melt()[nb_fu_by_aeid_melt()$aesae == 'nb.sae',]) + 
      geom_histogram(aes(x=count), fill=color_sae, binwidth = 0.5) +
      theme_bw() + labs(x="nb of SAE follow-ups", y="nb of events") + 
      scale_y_continuous(breaks = seq(0, sum(nb_fu_by_aeid_melt()$count), by = 1), minor_breaks = 0) +
      scale_x_continuous(breaks = seq(0, sum(nb_fu_by_aeid_melt()$count), by = 1), minor_breaks = 0)
    ggplotly(g, tooltip = NULL)
  })
  #plots violin + boxplot FU
  output$sae_fu_violin_2 <- renderPlotly({
    g <- ggplot(data=nb_fu_by_aeid_melt(), aes(x=label, y=count, color=label, fill=label)) + 
      geom_jitter(aes(color=label, text=paste0("Patient ID: ", mnpaid, "<br>AE ID: ", mnpaeid, "<br>nb follow-up: ", count)), 
                  width=0.2, height=0.05, alpha=0.5) + 
      geom_violin(alpha=0.1) +
      labs(x = "event type", y="nb follow-up", color="event type", fill="event type") +
      scale_color_manual(values=c("AE + SAE" = color_aesae, "AE" = color_ae, "SAE" = color_sae))+
      scale_fill_manual(values=c("AE + SAE" = color_aesae, "AE" = color_ae, "SAE" = color_sae))+
      scale_y_continuous(breaks = seq(0, max(nb_fu_by_aeid_melt()$count, na.rm = T), by=1), minor_breaks = 0, limits = c(0, NA)) + 
      theme_bw()
    p <- ggplotly(g, tooltip = "text")
    for (i in 1:length(p$x$data)){
      if(p$x$data[[i]]$mode == "lines"){
        p$x$data[[i]]$hoverinfo = "none" # remove density hover
      }
    }
    return(p)
  })
  output$sae_fu_boxplot_2 <- renderPlotly({
    g <- ggplot(data=nb_fu_by_aeid_melt(), aes(x=label, y=count, color=label, fill=label)) + 
      geom_jitter(aes(color=label, text=paste0("Patient ID: ", mnpaid, "<br>AE ID: ", mnpaeid, "<br>nb follow-up: ", count)), 
                  width=0.2, height=0.05, alpha=0.5) + 
      geom_boxplot(color="gray", fill=NA, alpha=0.1) +
      labs(x = "event type", y="nb follow-up", color="event type", fill="event type") +
      scale_color_manual(values=c("AE + SAE" = color_aesae, "AE" = color_ae, "SAE" = color_sae))+
      scale_fill_manual(values=c("AE + SAE" = color_aesae, "AE" = color_ae, "SAE" = color_sae))+
      scale_y_continuous(breaks = seq(0, max(nb_fu_by_aeid_melt()$count, na.rm = T), by=1), minor_breaks = 0, limits = c(0, NA)) + 
      theme_bw()
    p <- ggplotly(g, tooltip = "text")
    for (i in 1:length(p$x$data)){
      if(p$x$data[[i]]$type == "box"){
        #p$x$data[[i]]$hoverinfo = "none" # remove boxplot hover
        #p$x$data[[i]]$showlegend <- FALSE #remove legend for boxplot
      }
    }
    return(p)
  })
  output$sae_fu_table <- renderTable({
    return(nb_fu_by_aeid())
  })
}