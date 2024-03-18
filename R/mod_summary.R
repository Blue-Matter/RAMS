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

    shinydashboard::box(title='Metadata', width=4,
        uiOutput(ns('metadata'))
    ),
    shinydashboard::box(title='Risk Scores', width=8,
        DT::DTOutput(ns('summary'))
    )

  )
}

#' summary Server Functions
#'
#' @noRd
mod_summary_server <- function(id, objects){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$metadata <- renderUI({
      tagList(
        hr(),
        fluidRow(
          column(6,strong('Date Created:')),
          column(6,objects$metadata$Date)
        ),
        fluidRow(
          column(6,strong('Date Modified:')),
          column(6,objects$metadata$Date_Mod)
        ),
        fluidRow(
          column(6,strong('Creator:')),
          column(6,objects$metadata$Name)
        ),
        hr(),
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
        hr(),
        fluidRow(
          column(6,strong('Conservation Unit(s):')),
          column(6,objects$metadata$cus)
        ),
        fluidRow(
          column(6,strong('Watershed(s) or Population(s):')),
          column(6,objects$metadata$wss)
        ),
        hr(),
        fluidRow(
          column(6,strong('Note:')),
          column(6,objects$metadata$Note)
        ),
        hr()
      )
    })

    make_summary_table <- reactive({
      RAMS_scores <- objects$RAMS_scores

      LF <- dplyr::left_join(RAMS_scores, RAMS::LIMITING_FACTORS, by='LF_ID')

      displayDF <- LF |> dplyr::ungroup() |>
        dplyr::select(`Limiting Factor` = LF_ID,
                      `Life Stage`=Life.Stage,
                      Category=Limiting.Factor.Category,
                      Subcategory=Limiting.Factor.Subcategory,
                      Risk_Score=Risk_Score)

      displayDF$`Risk Category` <- unlist(score_categories[displayDF$Risk_Score])

      displayDF$`Life Stage` <- factor(displayDF$`Life Stage`)
      displayDF$Category <- factor(displayDF$Category)
      displayDF$Subcategory <- factor(displayDF$Subcategory)
      displayDF$`Risk Category` <- factor(displayDF$`Risk Category`)

      displayDF


    })

    output$summary <- DT::renderDT({
      displayDF <- make_summary_table()

      DT::datatable(displayDF, escape=FALSE, rownames = FALSE,

                    options=list(columnDefs=list(list(visible=FALSE, targets=4)),
                                 pageLength=10),
                    filter = list(
                      position = 'top', clear = FALSE)
      ) %>%
        DT::formatStyle('Risk Category', 'Risk_Score',
                        backgroundColor = DT::styleEqual(1:5, selectize_colors))



    })

  })

}





## To be copied in the UI
# mod_summary_ui("summary_1")

## To be copied in the server
# mod_summary_server("summary_1")

