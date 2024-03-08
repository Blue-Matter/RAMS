
#
# observeEvent(input$save_new, ignoreInit = TRUE, {
#   # if (check_metadata()) {
#   shiny::showModal(add_participants_Modal())
#   # }
#
# })
#
#
#
# add_participants_Modal <- function() {
#   modalDialog(
#     size='l',
#     title='Add Participants (optional)',
#     mod_add_participants_ui("add_participants_1"),
#     footer = tagList(
#       modalButton("Cancel"),
#       actionButton(ns("save_new"), "Save", icon=icon('save')),
#     )
#   )
# }



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
          column(4,
                 textInput(inputId = ns("first_name"), label = "", placeholder = 'First Name')
          ),
          column(4,
                 textInput(inputId = ns("last_name"), label = "", placeholder = 'Last Name')
          ),
          column(1,
                 shinyjs::hidden(
                   actionButton(ns('delete'), '', icon=icon('minus'),
                                style="color: #fff; background-color: #e63030; border-color: #2e6da4;
                     margin-top: 20px;")
                 )

                 )
          # column(3,
          #        textInput(inputId = ns("email"), label = "", placeholder = 'Email')
          # )
        )
      )

    })

    shinyjs::delay(60,
                   shinyjs::show('delete')
                   )

  })
}

#' add_participants Server Functions
#'
#' @noRd
mod_add_participants_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    n_participants_default <- 1


    initServers <- paste0('part_', 1:n_participants_default)
    part_IDs <- reactiveVal(initServers)

    for (mod_ids in 1:n_participants_default) {
      mod_participants_inputs_server(initServers[mod_ids])
    }

    output$participant_inputs <- renderUI({
      tagList()
      ll <- lapply(ns(part_IDs()), mod_participants_inputs_ui)
      do.call(tagList, ll)
    })


    n_participants <- reactiveVal(length(initServers))
    this_id <- reactiveVal(NULL)


    observeEvent(input$add_part, {
      this_id(paste0('part_', n_participants()+1))
      mod_participants_inputs_server(this_id())
      n_participants(n_participants()+1)
      part_IDs(append(part_IDs(), this_id()))

      insertUI(
        selector=paste0("#", ns(part_IDs()[length(part_IDs())-1]), '-participant'),
        where='afterEnd',
        ui=mod_participants_inputs_ui(ns(this_id()))
      )
    })

    observeEvent(input[[paste0(this_id(), '-delete')]], {
      removeUI(
        selector=paste0("#", ns(part_IDs()[length(part_IDs())]), '-participant'),
        immediate = TRUE
      )

    })



    output$participants <- renderUI({
      tagList(
        fluidRow(
          column(6,
                 br(),
                 p(strong('Upload a Participant List from a CSV file or enter participant details below'))
                 ),
          column(6,
                 shiny::fileInput('uploadparticipants', 'Upload CSV',
                                  accept='.csv', placeholder='Upload Participant List')
                 )
          ),
        fluidRow(
          column(1),
          column(4, p(strong('First Name'), style='text-align: center')),
          column(4, p(strong('Last Name'), style='text-align: center'))
          # column(3, p(strong('Email'), style='text-align: center')),
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
