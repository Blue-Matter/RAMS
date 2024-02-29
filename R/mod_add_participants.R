#' add_participants UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_add_participants_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('participants'))
  )
}

mod_participants_inputs_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('participant'))

}

mod_participants_inputs_server <- function(id){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    num <- strsplit(id, '_')[[1]]
    num <- num[length(num)]


    output$participant <- renderUI({
      tagList(
        fluidRow(
          column(1, style='text-align: right; margin-top:27px; padding-right:0px',
                 p(strong(num))
          ),
          column(3,
                 textInput(inputId = ns("first_name"), label = "", placeholder = 'First Name')
          ),
          column(3,
                 textInput(inputId = ns("last_name"), label = "", placeholder = 'Last Name')
          ),
          column(3,
                 textInput(inputId = ns("email"), label = "", placeholder = 'Email')
          )
        )
      )
    })

  })
}

#' add_participants Server Functions
#'
#' @noRd
mod_add_participants_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    n_participants_default <- 5


    part_IDs <- paste0('part_', 1:n_participants_default)
    for (mod_ids in 1:n_participants_default) {
      mod_participants_inputs_server(part_IDs[mod_ids])
    }

    output$participant_inputs <- renderUI({
      tagList()
      ll <- lapply(ns(part_IDs), mod_participants_inputs_ui)
      do.call(tagList, ll)
    })


    n_participants <- reactiveVal(length(part_IDs))

    observeEvent(input$add_part, {
      this_id <-  paste0('part_', n_participants()+1)
      mod_participants_inputs_server(this_id)
      n_participants(n_participants()+1)
      insertUI(
        selector=paste0("#", ns("add_part")),
        where='beforeBegin',
        ui=mod_participants_inputs_ui(ns(this_id))
      )

    })


    output$participants <- renderUI({
      tagList(
        actionButton('uploadparticipants', 'Upload Participants', icon=icon('upload'),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4;
                     margin-left: 50px"),
        br(),
        hr(),
        fluidRow(
          column(1),
          column(3, p(strong('First Name'), style='text-align: center')),
          column(3, p(strong('Last Name'), style='text-align: center')),
          column(3, p(strong('Email'), style='text-align: center')),
        ),

        uiOutput(ns('participant_inputs')),
        actionButton(ns('add_part'), 'Add Participant', icon=icon('plus'),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4;
                     margin-left: 50px")


      )
    })

  })
}

## To be copied in the UI
# mod_add_participants_ui("add_participants_1")

## To be copied in the server
# mod_add_participants_server("add_participants_1")
