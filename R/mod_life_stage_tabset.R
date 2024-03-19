get_limiting_factors <- function() {
  RAMS::LIMITING_FACTORS
}

# Limiting_Factors <- get_limiting_factors()



#' life_stage_tabset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_life_stage_tabset_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('life_stage_tabset'))
  )
}


#' life_stage_tabset Server Functions
#'
#' @noRd
mod_life_stage_tabset_server <- function(id, life_stage, life_stage_life_history=NULL,
                                         objects=NULL, home_session=NULL){

  LF_df <- get_limiting_factors() %>%
    dplyr::filter(Life.Stage ==life_stage,
                  Life.Stage.and.Life.History.Phase==life_stage_life_history)

  Life.Stage <- unique(LF_df$Life.Stage)
  Life.Stage.and.Life.History.Phase <- unique(LF_df$Life.Stage.and.Life.History.Phase)
  Life.History.Phase <- unique(LF_df$Life.History.Phase)
  Ecosystem.Unit <- unique(LF_df$Ecosystem.Unit)
  LF_categories <- unique(LF_df$Limiting.Factor.Category)

  ll <- list()
  for (i in seq_along(LF_categories)) {
    ll[[i]] <- categories_tabs(LF_df %>%
                                 dplyr::filter(Limiting.Factor.Category==LF_categories[i]))
  }

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$life_stage_tabset <- renderUI({
      tagList(
        shinydashboard::box(width=12, title=h3(paste('Life Stage: ', life_stage)),
            h4(paste('Ecosystem Unit: ', Ecosystem.Unit)),
            h4(paste('Life History Phase: ', Life.History.Phase)),
            br(),
            do.call(tabsetPanel, c(ll, list(id=id)))
        )
      )

    })
    # outputOptions(output, "life_stage_tabset", suspendWhenHidden = FALSE)




  })

  lf_ids <- LF_df$LF_ID
  IDs <- paste0('LF', lf_ids)
  lapply(IDs, mod_limiting_factor_server, objects=objects)

}

categories_tabs <- function(LF_DF=NULL) {
  sub_cats <- LF_DF$Limiting.Factor.Subcategory
  ll <- list()
  for (i in seq_along(sub_cats)) {
    lf_df <- LF_DF %>% dplyr::filter(Limiting.Factor.Subcategory==sub_cats[i])
    ll[[i]] <- sub_catories_tabs(lf_df)
  }

  tabPanel(unique(LF_DF$Limiting.Factor.Category),
           do.call(tabsetPanel, c(ll, list(type='pills'))))
}

sub_catories_tabs <- function(LF_DF=NULL) {
  id <- paste0('LF', LF_DF$LF_ID)
  tabPanel(unique(LF_DF$Limiting.Factor.Subcategory),
           mod_limiting_factor_ui(id)
  )

}


mod_limiting_factor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('limiting_factor'))
  )
}


mod_limiting_factor_server <- function(id, objects) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    LF <- reactive({
      as.numeric(gsub('LF', '', id))
    })

    get_scores <- function(name, initial=TRUE) {
      if (initial) {
        rams_scores <- objects$loaded_RAMS_scores
      } else {
        rams_scores <- objects$RAMS_scores
      }
      if (is.null(rams_scores)) {
        return(NULL)
      }
      tt <- rams_scores %>% dplyr::filter(LF_ID==LF())
      tt[name]
    }

    get_spatial <- reactive({
      get_scores('Spatial_Exposure')

    })

    get_temporal <- reactive({
      get_scores('Temporal_Exposure')
    })

    get_impact <- reactive({
      get_scores('Impact')
    })

    get_impact <- reactive({
      get_scores('Impact')
    })

    get_current_trend <- reactive({
      get_scores('Current_Trend')
    })

    get_future_trend <- reactive({
      get_scores('Future_Trend')
    })

    get_confidence <- reactive({
      get_scores('Confidence')
    })

    get_proof <- reactive({
      get_scores('Proof')
    })

    get_notes <- reactive({

      print('running get_scores from within get_notes')
      get_scores('Notes', FALSE)
    })

    update_RAMS_scores <- function(RAMS_scores, LF, Name, Value) {
      RAMS_scores[[Name]][LF] <- as.numeric(Value)
      RAMS_scores
    }

    spatial_scale <- mod_selectize_colored_server("spatial_scale", id,
                                                  'Spatial Exposure',
                                                  list('Low'=1,
                                                       'Moderate'=2,
                                                       'Medium'=3,
                                                       'High'=4,
                                                       'Very High'=5),
                                                  get_spatial)


    observeEvent(spatial_scale(), ignoreInit = TRUE, {
      objects$RAMS_scores <- update_RAMS_scores(objects$RAMS_scores, LF(),
                                                'Spatial_Exposure',
                                                spatial_scale())
    })


    temporal_scale <- mod_selectize_colored_server("temporal_scale", id,
                                                   'Temporal Exposure',
                                                   list('Low'=1,
                                                        'Moderate'=2,
                                                        'Medium'=3,
                                                        'High'=4,
                                                        'Very High'=5),
                                                   selected=get_temporal)

    observeEvent(temporal_scale(), ignoreInit = TRUE, {
      objects$RAMS_scores <- update_RAMS_scores(objects$RAMS_scores, LF(),
                                                'Temporal_Exposure',
                                                temporal_scale())

    })

    exposure_score <- reactive(calc_likelihood(spatial_scale(), temporal_scale()))

    mod_score_gauge_server("exposure_category", 'Exposure Score',
                           score_categories,
                           exposure_score,
                           selectize_colors)

    impact <- mod_selectize_colored_server("impact", id, 'Impact', list('Minor'=1,
                                                                    'Moderate'=2,
                                                                    'Major'=3,
                                                                    'Severe'=4,
                                                                    'Critical'=5),
                                           selected=get_impact)

    observeEvent(impact(), ignoreInit = TRUE, {
      objects$RAMS_scores <- update_RAMS_scores(objects$RAMS_scores, LF(),
                                                'Impact',
                                                impact())
    })

    risk_score <- reactive(calc_likelihood(exposure_score(), impact()))

    observeEvent(risk_score(), ignoreInit = TRUE, {
      objects$RAMS_scores <- update_RAMS_scores(objects$RAMS_scores, LF(),
                                                'Risk_Score',
                                                risk_score())

    })



    mod_score_gauge_server("risk_category", 'Risk Score',
                           score_categories,
                           risk_score,
                           selectize_colors)
    current_trend <- mod_selectize_server("current_trend", 'Current Trend', choices=list('Strongly Decreasing'=1,
                                                                                         'Somewhat Decreasing'=2,
                                                                                         'Stable'=3,
                                                                                         'Somewhat Increasing'=4,
                                                                                         'Strongly Increasing'=5),
                                          selected=get_current_trend)

    observeEvent(current_trend(), ignoreInit = TRUE, {
      objects$RAMS_scores <- update_RAMS_scores(objects$RAMS_scores, LF(),
                                                'Current_Trend',
                                                current_trend())
    })

    future_trend <- mod_selectize_server("future_trend", 'Future Trend', list('Strongly Decreasing'=1,
                                                                              'Somewhat Decreasing'=2,
                                                                              'Stable'=3,
                                                                              'Somewhat Increasing'=4,
                                                                              'Strongly Increasing'=5),
                                         selected=get_future_trend)

    observeEvent(future_trend(), ignoreInit = TRUE, {
      objects$RAMS_scores <- update_RAMS_scores(objects$RAMS_scores, LF(),
                                                'Future_Trend',
                                                future_trend())
    })

    future_risk_score <- reactive(calc_future_score(risk_score(), future_trend()))



    mod_score_gauge_server("future_category", 'Future Risk Score',
                           score_categories,
                           future_risk_score,
                           selectize_colors)

    confidence <- mod_selectize_server("confidence", 'Confidence Scale', list('Low'=1,
                                                                              'Medium'=2,
                                                                              'High'=3),
                                       get_confidence)

    observeEvent(confidence(), ignoreInit = TRUE, {
      objects$RAMS_scores <- update_RAMS_scores(objects$RAMS_scores, LF(),
                                                'Confidence',
                                                confidence())
    })

    proof <- mod_selectize_server("proof", 'Level of Proof', list('Hypothetical'=1,
                                                                  'Expert opinion'=2,
                                                                  'Derived information'=3,
                                                                  'Expanded observation'=4,
                                                                  'Empirical observation'=5),
                                  get_proof)

    observeEvent(proof(), ignoreInit = TRUE, {
      objects$RAMS_scores <- update_RAMS_scores(objects$RAMS_scores, LF(),
                                                'Proof',
                                                proof())
    })


    lf_id <- as.numeric(gsub(".*\\D", "", id, perl = TRUE))

    LF_DF <- get_limiting_factors() %>% dplyr::filter(LF_ID==lf_id)
    output$limiting_factor <- renderUI({

      tagList(
        shinydashboard::box(width=12,
                shinydashboardPlus::box(title='Biological Risk Scores', width=12, id='brs',
                                        solidHeader = TRUE,
                                        status = "primary",
                                        fluidRow(
                                          column(2, mod_selectize_colored_ui(ns("spatial_scale"))),
                                          column(2, mod_selectize_colored_ui(ns("temporal_scale"))),
                                          column(1),
                                          column(2, mod_selectize_colored_ui(ns("impact"))),
                                          column(1),
                                          column(2, mod_selectize_ui(ns("current_trend"))),
                                          column(2, mod_selectize_ui(ns("future_trend")))
                                        ),
                                        fluidRow(
                                          column(2, offset=1,
                                                 mod_score_gauge_ui(ns("exposure_category")),
                                                 align='center'
                                          ),
                                          column(2),
                                          column(2,
                                                 mod_score_gauge_ui(ns("risk_category")),
                                                 align='center'
                                          ),
                                          column(2, offset=2,
                                                 mod_score_gauge_ui(ns("future_category")),
                                                 align='center'
                                          )
                                        )
                ),
                shinydashboardPlus::box(title='Confidence', width=3,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        fluidRow(
                                          column(6, mod_selectize_ui(ns("confidence"))),
                                          column(6, mod_selectize_ui(ns("proof"))),
                                        ),
                                        fluidRow(
                                          column(12,
                                                 p(strong('Data Gaps')),
                                                 actionButton(ns('add_notes'), 'Add Notes', icon=icon('file-pen'))
                                          )

                                        )
                ),
                shinydashboardPlus::box(title='Correlated LFs', width=3,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        selectInput(ns('corr_LF'), 'Limiting Factors',
                                                    choices=c('LF 7: Hydrology & Oceanography - Low flows',
                                                              'LF 8: Water Quality - Temperature'),
                                                    multiple = TRUE)
                                        )
                )
            )
    })
    # outputOptions(output, "limiting_factor", suspendWhenHidden = FALSE)

    observeEvent(input$add_notes,ignoreInit = TRUE, {
      showModal(data_gaps_Modal())

    })

    data_gaps_Modal <- function() {
      modalDialog(
        textAreaInput(ns('data_gaps_notes'), 'Notes',
                      value=get_notes(),
                  placeholder = 'Add notes here on data gaps or other relevant information',
                  width='100%', height='200px'),
        size='l',
        footer = tagList(
          actionButton(ns("save_notes"), "Save", icon=icon('save')),
          modalButton("Cancel")

        )
      )
    }

    observeEvent(input$save_notes,ignoreInit = TRUE, {
      print(input$data_gaps_notes)
      objects$RAMS_scores <- update_RAMS_scores(objects$RAMS_scores, LF(),
                                                'Notes',
                                                input$data_gaps_notes)
      shiny::removeModal()
    })



  })
}
