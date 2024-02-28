#' selectize_colored UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_selectize_colored_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('selective_colored'))

  )
}

#' selectize_colored Server Functions
#'
#' @noRd
mod_selectize_colored_server <- function(id, parent_id, label, choices_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    bg <- reactive({
      select_color_css(parent_id, id, input[[id]], selectize_colors)
    })

    observeEvent(input[[id]], {
      bg()
    })

    output$background_change <- renderUI({
      tagList(fluidPage(tags$style(HTML(bg()))))
    })

    output$selective_colored <- renderUI({
      tagList(
        div(
          shinyhelper::helper(selectizeInput(ns(id) , with_red_star(label),
                                             choices=choices_list,
                                             selected=NULL,
                                             multiple = TRUE,
                                             options = list(maxItems = 1)
          ),
          icon = "question-circle",
          colour = "red",
          type = "markdown",
          content = id,
          size='l'),
          class=paste(parent_id, id,  'colored_selectInput')
        ),
        uiOutput(ns('background_change'))
      )
    })


    # disable the selectInputs when in View mode
    # observeEvent(input[[id]], {
    #
    #   if (ns(id)=='LF1-spatial_scale-spatial_scale') {
    #
    #     disid <- ns(id)
    #
    #     shinyjs::disable(disid, asis=TRUE)
    #
    #   }
    # })


    reactive(input[[id]])

  })
}

## To be copied in the UI
# mod_selectize_colored_ui("selectize_colored_1")

## To be copied in the server
# mod_selectize_colored_server("selectize_colored_1")
