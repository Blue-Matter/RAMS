#' links UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_links_ui <- function(id){
  ns <- NS(id)
  tagList(
    # fluidRow(
    # column(4,
    column(12,strong('Main Pages')),
    column(12, shiny::actionLink(ns('home'), 'Home', icon=icon('home'), width='100%')),
    column(12, shiny::actionLink(ns('summary'), 'Summary', icon=icon('list'), width='100%')),
    column(12, shiny::actionLink(ns('table'), 'Table', icon=icon('table'), width='100%')),

    column(12,strong('Scoring Pages')),
    column(12,shiny::actionLink(ns('egg_alevin'), 'Egg/Alevin', icon=icon('egg', class='fa-sm'))),
    column(12,shiny::actionLink(ns('fry_parr'), 'Fry/Parr',icon=icon('fish', class='fa-xs'))),
    column(12,shiny::actionLink(ns('smolt'), 'Smolt',icon=icon('fish', class='fa-sm'))),
    column(12,shiny::actionLink(ns('juvenile'), 'Juvenile',icon=icon('fish'))),
    column(12,shiny::actionLink(ns('immature'), 'Immature',icon=icon('fish', class='fa-lg'))),
    column(12,icon('fish', class='fa-xl', style='color: #3c8dbc;'),'Adult'),

    shiny::tags$ul(
      shiny::tags$li(shiny::actionLink(ns('return_migration'), 'Return Migration')),
      shiny::tags$li(shiny::actionLink(ns('terminal_migration'), 'Terminal Migration')),
      shiny::tags$li(shiny::actionLink(ns('spawning'), 'Spawning'))
    ),

    column(12,strong('Other Resources')),
    column(12,shiny::actionLink(ns('go_link_1'), 'Link 1')),
    column(12,shiny::actionLink(ns('go_link_2'), 'Link 2')),
    column(12,'These links can be specific to each page')

  )
}

#' links Server Functions
#'
#' @noRd
mod_links_server <- function(id, home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    make_link <- function(menuid) {
      js <- paste0('[data-value=\"', menuid, '\"]')
      shinyjs::runjs(paste0("$('a", js, "').tab('show');"))
    }


    observeEvent(input$home, {

      make_link('home')
    })

    observeEvent(input$summary, {
      make_link('summary')
    })

    observeEvent(input$table, {
      make_link('table')
    })

    observeEvent(input$egg_alevin, {
      make_link('egg_alevin')
    })

    observeEvent(input$fry_parr, {
      make_link('fry_parr')
    })

    observeEvent(input$smolt, {
      make_link('smolt')
    })

    observeEvent(input$juvenile, {
      make_link('juvenile')
    })

    observeEvent(input$immature, {
      make_link('immature')
    })

    observeEvent(input$return_migration, {
      make_link('return_migration')
    })

    observeEvent(input$terminal_migration, {
      make_link('terminal_migration')
    })

    observeEvent(input$spawning, {
      make_link('spawning')
    })

  })
}

## To be copied in the UI
# mod_links_ui("links_1")

## To be copied in the server
# mod_links_server("links_1")

