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


CK_CU_Info <- read.csv('CK_CU_Sites_En.csv')

CU_Info <- list(CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info)


Sites_Names <- list(CK_CU_Sites$SITE_NAME,
                 CK_CU_Sites$SITE_NAME,
                 CK_CU_Sites$SITE_NAME,
                 CK_CU_Sites$SITE_NAME,
                 CK_CU_Sites$SITE_NAME,
                 CK_CU_Sites$SITE_NAME,
                 CK_CU_Sites$SITE_NAME)

order_CU_Sites <- function(cu_info) {

  CU_codes <- cu_info$FULL_CU_IN
  CU_names <- cu_info$CU_NAME
  tt <- strsplit(CU_codes, '-')
  ord <- lapply(tt, '[[', 2) %>% unlist() %>% as.numeric()
  unique(paste(CU_codes[order(ord)], CU_names[order(ord)], sep=': '))
}

rel_pol_choices <- c('Fish Stock Provisions',
                     'Species at Risk Act',
                     'Wild Salmon Policy'
                     )

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
    selected_CUs <- reactive(input$cu_code_select)
    selected_WSs <- reactive(input$ws_code_select)

    cu_info <- reactive({
      req(selected_species())
      CU_Info[[as.numeric(selected_species())]]
    })

    cu_code_name <- reactive({
      req(selected_species())
      order_CU_Sites(cu_info())
    })


    sites_options <- reactive({
      req(selected_CUs())
      cus <- selected_CUs()
      cus <- unlist(lapply(strsplit(cus, ':'), '[[', 1))
      cu_info() %>% dplyr::filter(FULL_CU_IN %in% cus) %>%
        dplyr::pull(SITE_NAME)
    })

    observeEvent(selected_species(), {
      updateSelectInput(
        inputId='cu_code_select',
        choices = cu_code_name()
      )
    })

    observeEvent(selected_CUs(), {
      updateSelectInput(
        inputId='ws_code_select',
        choices = sites_options(),
        selected=selected_WSs()
      )
    })


    output$cu_code <- renderUI({
      req(selected_species())
      selectizeInput(ns('cu_code_select'),
                  with_red_star('Conservation Unit(s)'),
                  choices=NULL,
                  multiple=TRUE,
                  options=list(placeholder = 'Select CU code(s)'))
    })


    output$watershed <- renderUI({
      req(selected_CUs())
      selectizeInput(ns('ws_code_select'), with_red_star('Watershed(s) or Population(s)'),
                  choices=NULL,
                  multiple=TRUE,
                  options=list(create=TRUE,
                               placeholder = 'Select or Type New'))
    })

    output$new_dialog <- renderUI({
      tagList(
        fluidPage(
          fluidRow(
            column(3,
                   dateInput(ns('date'), 'Date',
                             value=Sys.Date()
                   )
            ),
            column(4,
                   textInput(ns('uoa'), with_red_star('Unit of Assessment'),
                             placeholder='e.g., Fall Cowichan Chinook'
                   )
            ),
            column(5,
                   selectizeInput(ns('rel_pol'),
                                  with_red_star('Relevant Legislation or Policy'),
                                  choices=rel_pol_choices,
                                  multiple=TRUE,
                                  options=list(create=TRUE,
                                               placeholder = 'Select or Type New')
                   )
            )
          ),
          fluidRow(
            column(3,
                   selectizeInput(ns('species') , with_red_star('Species'),
                                  choices=species_name_list,
                                  selected=NULL,
                                  multiple = TRUE,
                                  options = list(maxItems = 1,
                                                 placeholder = 'Select Species')
                   )
            ),
            column(5,
                   uiOutput(ns('cu_code'))
            ),
            column(4,
                   uiOutput(ns('watershed')))
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
