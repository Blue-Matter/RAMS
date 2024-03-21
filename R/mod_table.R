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
    shinydashboard::box(title=h3('Table'), width=12,
                        column(12,
                              p('This table shows all the scores for all the limiting factors. Scroll right on the table to see all the scores. Notes are not included here'),

                              p('Click on a row to go to the page for that LF'),
                              p('The table can be updated to show the score categories, and colour coded like the summary table '),
                              p('Another possibility could be to include drop down menus in each cell so that the scores can be updated here')
                        ),
                        shinydashboard::box(title=h4('Limiting Factors'), width=10,
                                            solidHeader = TRUE, status='primary',
                                            DT::DTOutput(ns('table'))
                        ),
                        shinydashboard::box(title=h4('Links'), width=2,
                                            solidHeader = TRUE, status='primary',
                                            collapsible = FALSE,
                                            mod_links_ui(ns(id))
                        ),
    )
  )
}


make_ordered_levels <- function(col) {
  factor(col, levels=unique(col),
         ordered=TRUE)
}
#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id, objects, home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_links_server(id, home_session)

    make_table <- reactive({
      RAMS_scores <- objects$RAMS_scores
      LF <- dplyr::left_join(RAMS::LIMITING_FACTORS, RAMS_scores, by='LF_ID') %>%
        dplyr::select(-RAMS_ID, -Life.Stage.and.Life.History.Phase, -menuid,
                      -Notes)

      LF$Ecosystem.Unit <- make_ordered_levels(LF$Ecosystem.Unit)
      LF$Life.Stage <- make_ordered_levels(LF$Life.Stage)
      LF$Life.History.Phase <- make_ordered_levels(LF$Life.History.Phase)
      LF$Limiting.Factor.Category <- make_ordered_levels(LF$Limiting.Factor.Category)
      LF$Limiting.Factor.Subcategory <- make_ordered_levels(LF$Limiting.Factor.Subcategory)

      col_names <- colnames(LF)
      col_names <- gsub('\\.', ' ', col_names)
      col_names <- gsub('_', ' ', col_names)
      col_names[1] <- "LF"
      DT::datatable(LF,
                    colnames = col_names,
                    rownames = FALSE,
                    filter = "top",
                    extensions = "FixedColumns",
                    options = list(autoWidth = FALSE,
                                   scrollX = TRUE,
                                   fixedColumns = list(leftColumns = 6),
                                   pageLength = 25,
                                   lengthMenu =  list(c(10, 25, 50, -1),
                                                      c("10", '25', '50', "All")),
                                   columnDefs = list(
                                     list(targets = "_all", className = "dt-center")
                                   )
                    ),
                    selection = list(mode = 'single', target='row')
      )

    })


    output$table <- DT::renderDT({
      make_table()
    })

    observeEvent(input$table_cell_clicked, {
      row <- input$table_cell_clicked$row
      col <- input$table_cell_clicked$col


      if (!is.null(row)) {
        LF <- RAMS::LIMITING_FACTORS

        js1 <- paste0('[data-value=\"', LF$menuid[row], '\"]')
        js2 <- paste0('[data-value=\"', LF$Limiting.Factor.Category[row], '\"]')
        js3 <- paste0('[data-value=\"', LF$Limiting.Factor.Subcategory[row], '\"]')

        shinyjs::runjs(paste0("$('a", js1, "').tab('show');"))
        shinyjs::delay(30, {
          shinyjs::runjs(paste0("$('a", js2, "').tab('show');"))
          shinyjs::runjs(paste0("$('a", js3, "').tab('show');"))
        }
        )


      }

    }, ignoreInit=TRUE)


  })
}



## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
