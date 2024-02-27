
create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                           <button class="btn btn-default action-button btn-primary action_button" id="load_',
                           .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-eye"></i></button>
                       </div>'
                     ))

  # purrr::map_chr(~
  #                  paste0(
  #                    '<div class = "btn-group">
  #                  <button class="btn btn-default action-button btn-primary action_button" id="load_',
  #                    .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-eye"></i></button>
  #                  <button class="btn btn-default action-button btn-info action_button" id="download_',
  #                    .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-download"></i></button>
  #                  <button class="btn btn-default action-button btn-danger action_button" id="delete_',
  #                    .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
  #                  ))
}


df <- read.csv('METADATA.csv') %>%
  dplyr::select(
    -idUSERS,
    -Date_Created,
    -Date_Modified,
    -RL)

colnames(df)[2:5] <- c(
  'Unit of Assessment',
  'Year',
  'Conservation Unit(s)',
  'Watershed or Population')

# add users
df$Facilitator <- c('A. Person', 'B. Person')



#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)

  tagList(
    shinydashboard::box(width=12, status='primary',
        title=h3('Welcome to the RAMS App'),
        shinydashboard::box(solidHeader = TRUE, status='primary', width=12,
            title='RAMS Database',
            column(9,
                   h4('Load an existing RAMS Process by clicking the',  icon('eye'), 'button',
                      'on a row in the table below')
                   ),
            column(3,
                   uiOutput(ns('new_button')),
                   style='float:right'),

            br(),
            br(),
            DT::dataTableOutput(ns('meta_data_table'))
            )
        )

  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, objects){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    show_new_button <- reactive({
      # if (objects$user_auth) {
        tagList(shinyWidgets::actionBttn(ns('create_new'), 'Create New RAMS',
                                         style = "jelly",
                                         color='primary',
                                         size='sm'))

      # } else {
      #   NULL
      # }
    })


    output$new_button <- renderUI({
      show_new_button()
    })


    make_meta_data_table <- function() {
      buttons <- create_btns(1:nrow(df))
      dplyr::bind_cols(df, tibble::tibble("Buttons" = buttons))
    }

    output$meta_data_table <- DT::renderDataTable({
      df <- make_meta_data_table()
      cnames <- colnames(df)
      DT::datatable(df,
                    selection='none',
                    escape=FALSE,
                    colnames = c(cnames[1:(ncol(df)-1)], ''),

                    options = list(
                      columnDefs = list(
                        list(orderable = F,
                             targets = c(ncol(df)))
                      )
                    )
      )
    })



    new_RAMS_modal <- function() {
      tagList(
        fluidPage(
          fluidRow(
            textInput('name', 'Name')
          )
        )

      )
    }

    create_new_Modal <- function() {
      modalDialog(
        title='New RAMS',
        new_RAMS_modal(),
        footer = tagList(
          actionButton(ns("save_new"), "Save", icon=icon('save')),
          modalButton("Cancel")
        )
      )
    }


    observeEvent(input$create_new, ignoreInit = TRUE, {
      shiny::showModal(create_new_Modal())

    })


    shiny::observeEvent(ns(input$create_new), {

      # modal popup

      # enter metadata

      # create RAMS object

      # save

      # display sidebar and expand



      # if (btn_type == 'load') {
      #  objects$selected_row <- selected_row
      #  objects$loaded <- TRUE
      # }

    })

    shiny::observeEvent(ns(input$current_id), {

      btn_pressed <- ns(input$current_id)
      btn_pressed <- gsub('home-', '', btn_pressed)

      btn_pressed <- strsplit(btn_pressed, '_')[[1]]
      btn_type <- btn_pressed[1]
      selected_row <- btn_pressed[2]

      if (!is.null(btn_type)) {
        if (btn_type == 'load') {
          objects$selected_row <- selected_row
          objects$loaded <- TRUE
        }
      }


    })

  })


}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
