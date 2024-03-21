#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(title=h3('Summary'), width=12,
                        column(12,
                               uiOutput(ns('metadata'))
                        ),
                        shinydashboard::box(title=h4('Risk Score Summary'), width=10,
                                            solidHeader = TRUE, status='primary',
                                            h4('This table shows the Risk Score for the Limiting Factors. Click on a cell to go to the RAMS score page.'),
                                            br(),
                                            DT::DTOutput(ns('summary'))
                        ),
                        shinydashboard::box(title=h4('Links'), width=2,
                                            solidHeader = TRUE, status='primary',
                                            collapsible = FALSE,
                                            mod_links_ui(ns(id))
                        ),
    )
  )
}





#' summary Server Functions
#'
#' @noRd
mod_summary_server <- function(id, objects, home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_links_server(id, home_session)
    output$metadata <- renderUI({
      tagList(
        column(6,
               fluidRow(
                 column(6,strong('Species:')),
                 column(6,objects$metadata$Species)
               ),
               fluidRow(
                 column(6,strong('Unit of Assessment:')),
                 column(6,objects$metadata$UOA)
               ),
               fluidRow(
                 column(6,strong('Relevant Policy or Legislation:')),
                 column(6,objects$metadata$rel_pol)
               ),
               fluidRow(
                 column(6,strong('Conservation Unit(s):')),
                 column(6,objects$metadata$cus)
               ),
               fluidRow(
                 column(6,strong('Watershed(s) or Population(s):')),
                 column(6,objects$metadata$wss)
               ),
               br()
        ),
        column(3,
               fluidRow(
                 column(6,strong('Creator:')),
                 column(6,objects$metadata$Name)
               ),
               fluidRow(
                 column(6,strong('Date Created:')),
                 column(6,objects$metadata$Date)
               ),
               fluidRow(
                 column(6,strong('Date Modified:')),
                 column(6,objects$metadata$Date_Mod)
               ),
               fluidRow(
                 column(6,strong('Notes:')),
                 column(6,objects$metadata$Note)
               ),
               br()
        )
      )
    })


    make_summary_table <- reactive({
      RAMS_scores <- objects$RAMS_scores
      LF <- dplyr::left_join(RAMS_scores, RAMS::LIMITING_FACTORS, by='LF_ID')

      Life.Stage.and.Life.History.Phase <- LF$Life.Stage.and.Life.History.Phase %>% unique()
      Limiting.Factor.Category <- LF$Limiting.Factor.Category %>% unique()

      LF$Limiting.Factor.Category <- factor(LF$Limiting.Factor.Category,
                                            levels=Limiting.Factor.Category,
                                            ordered = TRUE)

      columns <- LF %>% dplyr::arrange(Limiting.Factor.Category) %>%
        dplyr::distinct(Limiting.Factor.Category, Limiting.Factor.Subcategory)

      top_columns <- unique(columns$Limiting.Factor.Category)
      second_columns <- unique(columns$Limiting.Factor.Subcategory)

      # make matrix
      mat <- matrix(NA, nrow=length(Life.Stage.and.Life.History.Phase),
                    ncol=length(second_columns))
      colnames(mat) <- second_columns

      mat <- data.frame(mat, check.names = FALSE)
      DF <- dplyr::bind_cols(data.frame(Life.Stage.and.Life.History.Phase),
                              mat)

      for (i in 1:nrow(LF)){
        row_ind <- match(LF$Life.Stage.and.Life.History.Phase[i], Life.Stage.and.Life.History.Phase)
        col_ind <- match(LF$Limiting.Factor.Subcategory[i], second_columns)
        DF[row_ind, col_ind+1] <- score_categories[[LF$Risk_Score[i]]]

      }
      list(DF=DF, top_columns=top_columns, second_columns=second_columns)


    })

    background_color <- function(table, col_name) {
      DT::formatStyle(table, col_name,
                      backgroundColor = DT::styleEqual(score_categories,
                                                       selectize_colors))
    }

    observeEvent(input$summary_cell_clicked, {
      row <- input$summary_cell_clicked$row
      col <-  input$summary_cell_clicked$col + 1
      DF <- isolate(make_summary_table())
      DF <- DF$DF

      if (!is.null(row)) {
        RAMS_scores <- objects$RAMS_scores
        LF <- RAMS::LIMITING_FACTORS
        thisLF <- LF %>% dplyr::filter(Life.Stage.and.Life.History.Phase==DF[row,1],
                                        Limiting.Factor.Subcategory==colnames(DF)[col])

        js1 <- paste0('[data-value=\"', thisLF$menuid, '\"]')
        js2 <- paste0('[data-value=\"', thisLF$Limiting.Factor.Category, '\"]')
        js3 <- paste0('[data-value=\"', thisLF$Limiting.Factor.Subcategory, '\"]')

        shinyjs::runjs(paste0("$('a", js1, "').tab('show');"))
        shinyjs::delay(30, {
          shinyjs::runjs(paste0("$('a", js2, "').tab('show');"))
          shinyjs::runjs(paste0("$('a", js3, "').tab('show');"))
        }
        )


      }

    })

    output$summary <- DT::renderDT({
      obj <- make_summary_table()
      DF <- obj$DF
      top_columns <- obj$top_columns
      second_columns <- obj$second_columns


      sketch <- htmltools::withTags(
        table(
          class = "display",
          thead(
            tr(
              th(colspan = 1, rowspan=2,
                 'Life Stage and Life History Phase', style = "border-right: solid 2px;"),
              th(colspan = 2, top_columns[1], style = "border-right: solid 2px;", class='dt-center'),
              th(colspan = 4, top_columns[2], style = "border-right: solid 2px;", class='dt-center'),
              th(colspan = 4, top_columns[3], style = "border-right: solid 2px;", class='dt-center'),
              th(colspan = 3, top_columns[4], style = "border-right: solid 2px;", class='dt-center'),
              th(colspan = 4, top_columns[5], style = "border-right: solid 2px;", class='dt-center'),
              th(colspan = 2, top_columns[6], style = "border-right: solid 2px;", class='dt-center'),
            ),
            tr(
              th(colspan = 1, second_columns[1]),
              th(colspan = 1, second_columns[2], style = "border-right: solid 2px;"),
              th(colspan = 1, second_columns[3]),
              th(colspan = 1, second_columns[4]),
              th(colspan = 1, second_columns[5]),
              th(colspan = 1, second_columns[6], style = "border-right: solid 2px;"),
              th(colspan = 1, second_columns[7]),
              th(colspan = 1, second_columns[8]),
              th(colspan = 1, second_columns[9]),
              th(colspan = 1, second_columns[10], style = "border-right: solid 2px;"),
              th(colspan = 1, second_columns[11]),
              th(colspan = 1, second_columns[12]),
              th(colspan = 1, second_columns[13], style = "border-right: solid 2px;"),
              th(colspan = 1, second_columns[14]),
              th(colspan = 1, second_columns[15]),
              th(colspan = 1, second_columns[16]),
              th(colspan = 1, second_columns[17], style = "border-right: solid 2px;"),
              th(colspan = 1, second_columns[18]),
              th(colspan = 1, second_columns[18], style = "border-right: solid 2px;")
            )
          )
        )
      )

      dt <- DT::datatable(DF, escape=FALSE, rownames = FALSE, container=sketch,
                          colnames = second_columns,
                          extensions = "FixedColumns",
                          options = list(autoWidth = FALSE, scrollX = TRUE,
                                         fixedColumns = list(leftColumns = 1),
                                         info = FALSE,
                                         paging = FALSE,
                                         searching = FALSE,
                                         ordering=FALSE,
                                         columnDefs = list(
                                           list(targets = "_all", className = "dt-center")
                                         )
                          ),
                          selection = list(mode = 'single', target = 'cell'))

      for( x in second_columns){
        dt <- dt %>% background_color(., x)
      }


      dt

    })

  })

}





## To be copied in the UI
# mod_summary_ui("summary_1")

## To be copied in the server
# mod_summary_server("summary_1")

