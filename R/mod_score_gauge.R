
make_gauge <- function(value) {
  title_size <- 20
  if (value==0) color='white'
  if (value>0) color <- selectize_colors[value]

  fig <- plotly::plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    type = "indicator",
    mode = "gauge+number",
    # title=list(text=title, font=list(size=title_size, color='black')),
    number=list(font=list(color='black', size=title_size)),

    value = value,
    gauge = list(
      axis = list(range = list(0, 5), tickwidth = 1, nticks=6, tickcolor = "black"),
      bar = list(color = color, thickness=0.8),
      steps = list(range=0:5, thickness=0.8)
    )
  )  %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::layout(margin=list(l=5, r=5, t=5))

  fig

}

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

    # output$gauge_plot <- flexdashboard::renderGauge({
    #
    #   if(is.null(value())) {
    #     value <- 0
    #     color <- 'white'
    #     label <- ''
    #   } else {
    #     value <- as.numeric(value())
    #     color <- colors[value]
    #     label <- label[value]
    #   }
    #   flexdashboard::gauge(value, 0, 5, label=label,
    #                        sectors=flexdashboard::gaugeSectors(colors=color))
    # })

    output$gauge_plot <- plotly::renderPlotly({

      if(is.null(value())) {
        value <- 0
        color <- 'white'
        label <- ''
      } else {
        value <- as.numeric(value())
        color <- colors[value]
        label <- label[value]
      }
      make_gauge(value)
    })

    get_score_category <- reactive({
      val <- value()
      if (is.null(val))
        return(NULL)
      score_categories[[val]]
    })




    output$gaugeplot <- renderUI({

      tagList(
        # flexdashboard::gaugeOutput(ns('gauge_plot'), height='110px')
        h4(strong(heading), style='text-align: center;'),
        plotly::plotlyOutput(ns('gauge_plot'), height='200px'),
        h4(get_score_category(), style='text-align: center;')
      )

    })
   # outputOptions(output, "gaugeplot", suspendWhenHidden = FALSE)

  })
}

## To be copied in the UI
# mod_score_gauge_ui("score_gauge_1")

## To be copied in the server
# mod_score_gauge_server("score_gauge_1")
