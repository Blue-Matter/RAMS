#' sidebar_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_main_ui <- function(id){
  ns <- NS(id)
  tagList(

    shinydashboard::sidebarMenuOutput(ns('sidebar'))


  )
}

#' sidebar_main Server Functions
#'
#' @noRd
mod_sidebar_main_server <- function(id, objects){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    menu_list <- reactive({

      ll <- list(shinydashboard::menuItem('Home', tabName='home', icon=icon('home')))

      if (objects$loaded) {
        ll <- list(shinydashboard::menuItem('Home', tabName='home', icon=icon('home')),
                   shinydashboard::menuItem('Egg / Alevin', tabName = 'egg_alevin', icon=icon('egg', class='fa-sm')),
                   shinydashboard::menuItem('Fry / Parr', tabName='fry_parr', icon=icon('fish', class='fa-xs')),
                   shinydashboard::menuItem('Smolt', tabName='smolt', icon=icon('fish', class='fa-sm')),
                   shinydashboard:: menuItem('Juvenile', tabName='juvenile', icon=icon('fish')),
                   shinydashboard::menuItem('Immature', tabName='immature', icon=icon('fish', class='fa-lg')),
                   shinydashboard:: menuItem('Adult', startExpanded = TRUE, icon=icon('fish', class='fa-xl'),
                                             shinydashboard::menuSubItem('Return Migration', tabName='return_migration'),
                                             shinydashboard::menuSubItem('Terminal Migration', tabName='terminal_migration'),
                                             shinydashboard::menuSubItem('Spawning', tabName='spawning')
                   )
        )
      }
      ll
    })

    output$sidebar <- shinydashboard::renderMenu({
      sidebarMenu(.list=menu_list())
    })

  })
}

## To be copied in the UI
# mod_sidebar_main_ui("sidebar_main_1")

## To be copied in the server
# mod_sidebar_main_server("sidebar_main_1")
