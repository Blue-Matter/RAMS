#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}



## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
