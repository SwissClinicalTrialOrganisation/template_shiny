#' Recruitment module Functions
#'
#' @description This module presents a recruitment plot and table using the accrualPlot package
#' @rdname mod_recruitment2
#' @param id,input,output,session,label Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom accrualPlot accrual_create_df gg_accrual_plot_cum accrual_table
#' @importFrom gt render_gt gt tab_options
#' @importFrom plotly ggplotly renderPlotly layout
#'

mod_recruitment2_ui <- function(id, label){

  ns <- NS(id)

  tabItem(tabName = label,

          ##
          tags$style(HTML("


      .box.box-solid.box-primary>.box-header {
      color:#fff;
      background:#1ea5a5
                    }

      .box.box-solid.box-primary{
      border-bottom-color:#1ea5a5;
      border-left-color:#1ea5a5;
      border-right-color:#1ea5a5;
      border-top-color:#1ea5a5;
                    }

                  ")),

          # fluidRow(
          #   tags$style(".small-box.bg-yellow { background-color: #a5d7d2 !important; color: #333333 !important; }"),
          #   tags$style(".small-box.bg-red { background-color: #d20537 !important; color: #333333 !important; }"),
          #   valueBoxOutput(ns("randomized"), width = 6),
          #   valueBoxOutput(ns("estimated"), width = 6)
          # ),
          # 
          # uiOutput(ns("targetsUI")),

          fluidRow(
            tabBox(
              width = 12,
              title = "",
              id = "tabset1", height = "450px",
              selected = "Recruitment over time by center",
              tabPanel("Recruitment over time by center",
                       plotlyOutput(ns('recruitplot'), height = "400"),
                       tags$br()
                       )

            )
          )
      # ,
      #     tags$br(),
      #     tags$br(),
      #     fluidRow(gt::gt_output(ns('recruittable')))
  )
}

#' recruitment Server Function
#' @rdname mod_recruitment2
#' @param data.randomized reactive data containing randomization info
#'   (see {mod_recruitment_prediction} for example without reactivity)
#'
mod_recruitment2_server <- function(input, output, session, data.randomized, locations, all_data){

  ns <- session$ns

  # output$randomized <- renderValueBox({
  # 
  # 
  #   perc <- round(nrow(data.randomized())/150*100, digits = 1)
  #   valueBox(value = paste0(nrow(data.randomized()), " out of ", "150", " (", perc, "%)" ),
  #            subtitle = "Randomized from pre-registered patients (those with inadequate skin flaps are not randomized)",
  #            icon = icon("user-plus"), color = "yellow")
  # })
  # 
  # output$estimated <- renderValueBox({
  # 
  #   perc <- round(nrow(data.randomized())/300*100, digits = 1)
  #   valueBox(value = paste0(nrow(data.randomized()), " out of ", 300, " (", perc, "%)" ),
  #            subtitle = "actual out of estimated recruitment", icon = icon("user-plus"), color = "yellow")
  # })
# 
  acc <- reactive({
    accrualPlot::accrual_create_df(data.randomized()$rando_date.date,
                                   by = data.randomized()$centre.short)
  })
#   acc2 <- accrualPlot::accrual_create_df(all_data$rando_date.date,
#                                    by = all_data$centre.short)


  ## Recruitment plot
  output$recruitplot <- renderPlotly({

    p <- accrualPlot::gg_accrual_plot_cum(acc()) +
      theme_bw() +
      scale_x_date(labels = function(x) format(x, format = "%d %b %Y"))

    plotly::ggplotly(p) %>% layout(legend = list(y = 0.5))
  })

  # output$recruittable <- gt::render_gt({
  #   gt::gt(accrualPlot::accrual_table(acc(), unit = "month", format_table_date = "%b %Y")) %>%
  #     gt::tab_options(column_labels.hidden = TRUE)
  # })
  # 
  # # recruitment targets
  # targets <- locations %>%
  #   dplyr::filter(centre.short %in% names(acc2))
  # 
  # tmp <- lapply(seq_along(acc2), function(x){
  #     acc2 %>%
  #       "[["(x) %>%
  #       # number this month
  #       # dplyr::filter(Date >= Sys.Date() - 30.4) %>%
  #       dplyr::filter(Date >= as.Date('2022/03/01') - 30.4) %>%
  #       dplyr::summarize(n = sum(Freq)) %>%
  #       dplyr::mutate(centre.short = names(acc2)[x])
  #       # names()
  #     }) %>%
  #     dplyr::bind_rows() %>%
  #     # add in the target
  #     dplyr::left_join(dplyr::bind_rows(targets,
  #                                       # overall target is the sum of all
  #                                       targets %>%
  #                                         dplyr::summarize(monthly = sum(monthly)) %>%
  #                                         dplyr::mutate(centre.short = "Overall"))) %>%
  #     dplyr::mutate(colour = dplyr::case_when(n / monthly < 0.5 ~ "red",
  #                                             n / monthly < 1 ~ "orange",
  #                                             TRUE ~ "green"))
  # 
  # output$targetsUI <- renderUI({
  #   fluidRow(
  #     lapply(1:nrow(tmp), function(x){
  #       uiname <- paste0("target_", x)
  #       message(x)
  #       # output[[uiname]] <- renderInfoBox(
  #       infoBox(title = tmp$centre.short[x],
  #               value = paste0(tmp$n, " / ", tmp$monthly)[x],
  #               color = tmp$colour[x],
  #               icon = shiny::icon('bullseye'),
  #               fill = TRUE)
  #       # )
  #     })
  #   )
  # })

}



