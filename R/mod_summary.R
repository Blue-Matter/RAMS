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

    shinydashboard::box(title='Metadata', width=12,
                        collapsible = TRUE,
        uiOutput(ns('metadata'))
    ),
    shinydashboard::box(title='Risk Scores', width=12,
        DT::DTOutput(ns('summary'))
    )

  )
}

#' summary Server Functions
#'
#' @noRd
mod_summary_server <- function(id, objects, home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$metadata <- renderUI({
      tagList(
        hr(),
        column(4,
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
               )
        ),
        column(4,
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
               )
        ),
        column(4,
               fluidRow(
                 h3('Links'),
                 h4('Life Stages:'),
                 actionButton(ns('egg_alevin'), 'Egg/Alevin'),
                 actionButton(ns('fry_parr'), 'Fry/Parr')
               )
        ),
        hr()
      )
    })



    # observeEvent(links$link, {
    #   print('button pressed')
    #   updateTabsetPanel(home_session, 'menu_sidebar', 'egg_alevin')
    #   updateTabsetPanel(session, inputId='egg_alevin', selected='Water Quality')
    # })
    #
    #
    observeEvent(input$egg_alevin, {
      updateTabsetPanel(home_session, 'menu_sidebar', 'egg_alevin')
    })

    observeEvent(input$fry_parr, {
      updateTabsetPanel(home_session, 'menu_sidebar', 'fry_parr')
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

      mat <- data.frame(mat)
      DF <- dplyr::bind_cols(data.frame(Life.Stage.and.Life.History.Phase),
                              mat)

      for (i in 1:nrow(LF)){
        row_ind <- match(LF$Life.Stage.and.Life.History.Phase[i], Life.Stage.and.Life.History.Phase)
        col_ind <- match(LF$Limiting.Factor.Subcategory[i], second_columns)

        DF[row_ind, col_ind+1] <- score_categories[[LF$Risk_Score[i]]]
      }

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

      background_color <- function(table, col_name) {
        col <- gsub(' ', '.', col_name)
        col <- gsub(',', '.', col)
        col <- gsub('\\(', '.', col)
        col <- gsub('\\)', '.', col)
        DT::formatStyle(table, col,
                        backgroundColor = DT::styleEqual(score_categories,
                                                         selectize_colors))
      }


      dt <- DT::datatable(DF, escape=FALSE, rownames = FALSE, container=sketch,
                    colnames = second_columns,
                    options = list(autoWidth = FALSE, scrollX = TRUE,
                                   info = FALSE,
                                   paging = FALSE,
                                   searching = FALSE,
                                   ordering=FALSE,
                                   columnDefs = list(
                                     list(targets = "_all", className = "dt-center")
                      )
                    ))

      for( x in second_columns){
        dt <- dt %>% background_color(., x)
      }


     dt

    })

    output$summary <- DT::renderDT({
      make_summary_table()

    })

  })

}





## To be copied in the UI
# mod_summary_ui("summary_1")

## To be copied in the server
# mod_summary_server("summary_1")

