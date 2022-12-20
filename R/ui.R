#' The application User-Interface
#'
#' @import lubridate
#' @import shiny
#' @import shinydashboard
#'
#' @noRd

app_ui <- function() {



    data <- get_data()
    ## Import module label ids
    mod <- get_modules()

    ## List the first level UI elements here
    fluidPage(

      dashboardPage(skin = "blue",

                    ## Header
                    dashboardHeader(title = paste0("Study as of ",
                                                   data$data.extraction.date
                                                   ), titleWidth = 300),

                    ## Sidebar
                    dashboardSidebar(

                      tags$head(tags$style(HTML('.logo {
                              background-color: #1ea5a5 !important;

                              }
                              .navbar {
                              background-color: #1ea5a5  !important;

                              }

                              .navbar-custom .navbar-text {
                                color: black;
                              }
                              '))),

                      sidebarMenu(
                        ## Home
                        menuItem("Home", tabName = mod$home, icon = icon("home")),

                        ## Overview tab
                        menuItem("Performance measures", startExpanded = TRUE,
                                 ## 1st SIDEBAR TAB: Recruitment and retention
                        #          menuItem("Recruitment", tabName = mod$recruit, icon = icon("chart-line")),
                                 menuItem("Recruitment (accrualPlot)", tabName = mod$recruit2, icon = icon("chart-line"))
                        # ,
                        #          menuItem("Recruitment map", tabName = mod$recruitmap, icon = icon("map")),
                        #          menuItem("Recruitment completion", tabName = mod$recruitment_prediction, icon = icon("chart-line")),
                        #          menuItem("Retention", tabName = mod$retention, startExpanded = TRUE, icon = icon("door-open")),
                        #          menuItem("Data quality", startExpanded = TRUE, icon = icon("database"),
                        #                   menuItem("Completeness", tabName = mod$completeness),
                        #                   menuItem("Consistency", tabName = mod$consistency),
                        #                   menuItem("Timeliness", tabName = mod$timeliness),
                        #                   menuItem("Queries", tabName = mod$queries))),
                        # menuItem("Study management", startExpanded = TRUE,
                        #          ## 1st SIDEBAR TAB: Recruitment and retention
                        #          menuItem("Follow-up visits", tabName = mod$visits, icon = icon("clinic-medical")),
                        #          menuItem("Participant characteristics", tabName = mod$participant, icon = icon("address-card")),
                        #          menuItem("Safety management", startExpanded = TRUE, icon = icon("notes-medical"),
                        #                   menuItem("Serious adverse events", tabName = mod$sae),
                        #                   menuItem("Adverse events", tabName = mod$ae),
                        #                   menuItem("Annual safety report", tabName = mod$asr)
                        # 
                        #                   )
                        ),

                        ## Date range filter
                        dateRangeInput("period", "Randomization date:",
                                       start = as.POSIXct("2017-07-01"),
                                       end   = as.POSIXct(today())),
                        ## Center filter
                        selectInput("center", "Acute center", choices = c("All", "A", "B", "C", "D", "E"), selected = "All"),
                        
                        width = "350")),

                    ## Body
                    dashboardBody(
                      
                      # Make sure the calendar input of dateRangeInput is not hidden behind the dashboard header
                      tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),

                      tabItems(

                        ## Recruitment tab
                        mod_home_ui(mod$home, label = mod$home),
                        # mod_recruitment_ui(mod$recruit, label = mod$recruit),
                        mod_recruitment2_ui(mod$recruit2, label = mod$recruit2)
                        # ,
                        # mod_recruitment_map_ui(mod$recruitmap, label = mod$recruitmap)
                        # , mod_recruitment_prediction_ui(mod$recruitment_prediction, label = mod$recruitment_prediction)
                        # , mod_retention_ui(mod$retention, label = mod$retention)
                        # , mod_completeness_ui(mod$completeness, label = mod$completeness)
                        # , mod_consistency_ui(mod$consistency, label = mod$consistency)
                        # , mod_timeliness_ui(mod$timeliness, label = mod$timeliness)
                        # , mod_queries_ui(mod$queries, label = mod$queries)
                        # , mod_visits_ui(mod$visits, label = mod$visits)
                        # , mod_participant_ui(mod$participant, label = mod$participant)
                        # , mod_sae_ui(mod$sae, label = mod$sae)
                        # , mod_ae_ui(mod$ae, label = mod$ae)
                        # , mod_asr_ui(mod$asr, label = mod$asr)

                      )
                    ) ## dashboardBody
      ) ## dashboardPage
    )



}


