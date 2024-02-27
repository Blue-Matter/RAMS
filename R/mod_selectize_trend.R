#' selectize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_selectize_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('selectize_trend'))

  )
}

#' selectize Server Functions
#'
#' @noRd
mod_selectize_server <- function(id, label, choices){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$selectize_trend <- renderUI({
      tagList(
        div(
          shinyhelper::helper(selectizeInput(ns(id) , with_red_star(label),
                                             choices=choices,
                                             multiple = TRUE,
                                             options = list(maxItems = 1)
          ),
          icon = "question-circle",
          colour = "red",
          type = "markdown",
          content = id,
          size='l'),
          class=paste(id)
        )
      )
    })
    reactive(input[[id]])
  })
}


