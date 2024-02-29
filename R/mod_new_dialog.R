species_name_list <- list('Chinook'=1,
                  'Chum'=2,
                  'Coho'=3,
                  'Even Year Pink'=4,
                  'Odd Year Pink'=5,
                  'Lake Type Sockeye'=6,
                  'River Type Sockeye'=7
                  )

species_list <- list('Oncorhynchus tshawytscha'=1,
                  'Oncorhynchus keta'=2,
                  'Oncorhynchus kisutch'=3,
                  'Oncorhynchus gorbuscha'=4,
                  'Oncorhynchus gorbuscha'=5,
                  'Oncorhynchus nerka'=6,
                  'Oncorhynchus nerka'=7
                  )


CK_CU_Sites <- read.csv('CK_CU_Sites_En.csv')
CU_Sites <- list(CK_CU_Sites$FULL_CU_IN,
                 CK_CU_Sites$FULL_CU_IN,
                 CK_CU_Sites$FULL_CU_IN,
                 CK_CU_Sites$FULL_CU_IN,
                 CK_CU_Sites$FULL_CU_IN,
                 CK_CU_Sites$FULL_CU_IN,
                 CK_CU_Sites$FULL_CU_IN)


order_CU_Sites <- function(CU_codes) {
  tt <- strsplit(CU_codes, '-')
  ord <- lapply(tt, '[[', 2) %>% unlist() %>% as.numeric()
  unique(CU_codes[order(ord)])
}

#' new_dialog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_new_dialog_ui <- function(id){
  ns <- NS(id)
  tagList(

    uiOutput(ns('new_dialog'))

  )
}

#' new_dialog Server Functions
#'
#' @noRd
mod_new_dialog_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    selected_species <- reactive(input$species)

    cu_codes <- reactive({
      order_CU_Sites(CU_Sites[[as.numeric(selected_species())]])
    })

    observeEvent(selected_species(), {
      updateSelectInput(
        inputId='cu_code_select',
        choices = cu_codes(),
        selected=1
      )
    })


    output$cu_code <- renderUI({
      req(selected_species())
      selectInput(ns('cu_code_select'), with_red_star('CU Code'),
                  choices=NULL,
                  multiple=TRUE)
    })

    output$new_dialog <- renderUI({
      tagList(
        fluidPage(
          fluidRow(
            column(3,
                   dateInput(ns('date'), with_red_star('Date'),
                             value=Sys.Date())
            ),
            column(3,
                   selectizeInput(ns('participants'), 'Participants',
                                  choices=NULL,
                                  selected=NULL,
                                  multiple = TRUE,
                                  options = list(create = TRUE))
                   )
            ),

          fluidRow(
            column(3,
                   selectizeInput(ns('species') , with_red_star('Species Name'),
                                  choices=species_name_list,
                                  selected=NULL,
                                  multiple = TRUE,
                                  options = list(maxItems = 1)
                   )
            ),
            column(3,
                   uiOutput(ns('cu_code'))
            )

          )

        )
      )
    })
  })
}



## To be copied in the UI
# mod_new_dialog_ui("new_dialog_1")

## To be copied in the server
# mod_new_dialog_server("new_dialog_1")
