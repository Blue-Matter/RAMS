#' score_gauge UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_score_gauge_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('gaugeplot'))
  )
}

#' score_gauge Server Functions
#'
#' @noRd
mod_score_gauge_server <- function(id, heading, label, value, colors){

  stopifnot(is.reactive(value))

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$gauge_plot <- flexdashboard::renderGauge({

      if(is.null(value())) {
        value <- 0
        color <- 'white'
        label <- ''
      } else {
        value <- as.numeric(value())
        color <- colors[value]
        label <- label[value]
      }
      flexdashboard::gauge(value, 0, 5, label=label,
                           sectors=flexdashboard::gaugeSectors(colors=color))
    })

    output$gaugeplot <- renderUI({

      tagList(
        flexdashboard::gaugeOutput(ns('gauge_plot'), height='110px'),
        p(strong(heading))
      )

    })

  })
}

## To be copied in the UI
# mod_score_gauge_ui("score_gauge_1")

## To be copied in the server
# mod_score_gauge_server("score_gauge_1")
