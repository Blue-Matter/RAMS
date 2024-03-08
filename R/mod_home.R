

species_name_list <- c('Chinook',
                          'Chum',
                          'Coho',
                          'Even Year Pink',
                          'Odd Year Pink',
                          'Lake Type Sockeye',
                          'River Type Sockeye'
)

species_list <- c('Oncorhynchus tshawytscha',
                     'Oncorhynchus keta',
                     'Oncorhynchus kisutch',
                     'Oncorhynchus gorbuscha',
                     'Oncorhynchus gorbuscha',
                     'Oncorhynchus nerka',
                     'Oncorhynchus nerka'
)


CK_CU_Info <- read.csv('CK_CU_Sites_En.csv')

CU_Info <- list(CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info,
                CK_CU_Info)


order_CU_Sites <- function(cu_info) {

  cu_info_df <- data.frame(Code=cu_info$FULL_CU_IN, Name=cu_info$CU_NAME) %>%
    dplyr::distinct()

  tt <- strsplit(cu_info_df$Code, '-')
  ord <- lapply(tt, '[[', 2) %>% unlist() %>% as.numeric()
  cu_info_df <- cu_info_df[order(ord),]

  ll <- list()
  for (i in 1:nrow(cu_info_df)) {
    ll[[paste(cu_info_df[i,], collapse=': ')]] <- cu_info_df$Code[i]
  }
  ll
}

rel_pol_choices <- c('Fish Stock Provisions',
                     'Species at Risk Act',
                     'Wild Salmon Policy'
)

create_new_Modal <- function(ns) {
  modalDialog(
    size='l',
    title='New RAMS Process',
    uiOutput(ns('new_dialog')),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("save_new"), "Save", icon=icon('save'))
    )
  )
}

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


make_meta_data_table <- function(df) {
  buttons <- create_btns(1:nrow(df))
  dplyr::bind_cols(df, tibble::tibble("Buttons" = buttons))
}



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
                        title=h3('Welcome to the Risk Assessment Method for Salmon App'),
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
mod_home_server <- function(id, objects, credentials){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$new_button <- renderUI({
      if (credentials()$user_auth) {
        tagList(shinyWidgets::actionBttn(ns('create_new'), 'Create New RAMS',
                                         style = "jelly",
                                         color='primary',
                                         size='sm'))
      } else {
        NULL
      }
    })

    output$meta_data_table <- DT::renderDataTable({
      df <- make_meta_data_table(df)
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
                   shinyWidgets::pickerInput(ns('species') ,
                                             with_red_star('Species'),
                                             choices=species_name_list,
                                             options = list(
                                               title = "Select Species")

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

    selected_species <- reactive(input$species)
    selected_CUs <- reactive(input$cu_code_select)
    selected_WSs <- reactive(input$ws_code_select)

    cu_info <- reactive({
      req(selected_species())
      ind <- which(species_name_list == selected_species())
      CU_Info[[ind]]
    })

    cu_code_name <- reactive({
      req(selected_species())
      order_CU_Sites(cu_info())
    })

    sites_options <- reactive({
      req(selected_CUs())
      cus <- selected_CUs()
      cus <- unlist(lapply(strsplit(cus, ':'), '[[', 1))
      df <- cu_info()
      ll <- list()
      for (i in seq_along(cus)) {
        cu <- cus[i]
        ll[[cu]] <- df  %>% dplyr::filter(FULL_CU_IN == cu) %>%
          dplyr::pull(SITE_NAME)
        if (length(ll[[cu]])==1) {
          names(ll)[i] <- paste(cu, ll[[cu]], sep=': ')
        }
      }
      ll
    })

    output$cu_code <- renderUI({
      # req(selected_species())
      shinyWidgets::pickerInput(
        ns('cu_code_select'),
        with_red_star('Conservation Unit(s)'),
        choices=NULL,
        multiple=TRUE
      )
    })

    observeEvent(selected_species(), {
      shinyWidgets::updatePickerInput(
        inputId='cu_code_select',
        choices = cu_code_name()
      )
    })

    output$watershed <- renderUI({
      # req(selected_CUs())
      shinyWidgets::pickerInput(
        inputId = ns('ws_code_select'),
        label = with_red_star('Watershed(s) or Population(s)'),
        multiple = TRUE,
        choices = NULL,
        options = list(
          `actions-box` = TRUE)

      )
    })

    observeEvent(selected_CUs(), {
      shinyWidgets::updatePickerInput(inputId='ws_code_select',
                                      choices=sites_options(),
                                      selected=selected_WSs())
    })

    observeEvent(input$create_new, ignoreInit = TRUE, {
      shiny::showModal(create_new_Modal(ns))
    })


    # TEMP ID
    id_rams <- 1
    Date_UOA_Species_List <- reactiveValues(ID_RAMS=id_rams,
                                            Date=NULL,
                                            `Unit of Assessment`=NULL,
                                            Species=NULL)


    observeEvent(input$date, Date_UOA_Species_List$Date <- input$date)
    observeEvent(input$uoa, Date_UOA_Species_List$`Unit of Assessment` <- input$uoa)
    observeEvent(input$species, Date_UOA_Species_List$Species <- selected_species())

    Rel_Pol_List <- reactiveValues(ID_RAMS=id_rams,
                                   `Relevant Legislation or Policy`=NULL)

    observeEvent(input$rel_pol, Rel_Pol_List$`Relevant Legislation or Policy` <- input$rel_pol)


    CU_List <- reactiveValues(ID_RAMS=id_rams,
                                 `Conservation Unit(s)`=NULL)

    observeEvent(input$cu_code_select, CU_List$`Conservation Unit(s)` <- selected_CUs())

    WS_List <- reactiveValues(ID_RAMS=id_rams,
                              `Conservation Unit(s)`=NULL,
                              `Watershed(s) or Population(s)`=NULL)


    observeEvent(input$ws_code_select,  {
      req(input$cu_code_select)
      SITES <- sites_options()
      names(SITES) <- selected_CUs()
      CUS <- selected_CUs()
      WSS <- selected_WSs()

      match_CU <- function(x, lst) {
        for (i in seq_along(lst)) {
          if (x %in% lst[[i]])
            break()
        }
        names(lst)[i]
      }

      cu_list <- list()
      for (i in seq_along(WSS)) {
        cu_list[[i]] <- match_CU(x=WSS[i], lst=SITES)
      }

      WS_List$`Conservation Unit(s)` <- unlist(cu_list)
      WS_List$ `Watershed(s) or Population(s)` <- selected_WSs()

    })


    check_list <- function(ll) {
      chk <- lapply(ll, shiny::isTruthy)
      missing <- which(!unlist(chk))
      if (length(missing)>0)
        return(names(missing))
      return(NULL)
    }

    check_meta_inputs <- function(meta_inputs_list) {
      lapply(meta_inputs_list, check_list) %>% unlist() %>% unique()

    }

    check_metadata <- function(Date_UOA_Species_List,
                               Rel_Pol_List,
                               CU_List,
                               WS_List){
      meta_inputs_list <- list(reactiveValuesToList(Date_UOA_Species_List),
                               reactiveValuesToList(Rel_Pol_List),
                               reactiveValuesToList(CU_List),
                               reactiveValuesToList(WS_List)
      )

      chk <- reactive(check_meta_inputs(meta_inputs_list))
      if (length(chk())>0) {
        shinyalert::shinyalert('Missing Inputs',
                               tagList(list_to_p(chk())),
                               type='error',
                               closeOnClickOutside =TRUE,
                               html=TRUE)
        return(FALSE)
      }
      TRUE
    }

    observeEvent(input$save_new, ignoreInit = TRUE, {
      if (check_metadata(Date_UOA_Species_List, Rel_Pol_List, CU_List, WS_List)) {
        shiny::removeModal()

        # modal popup

        # enter metadata

        # create RAMS object

        # save

        # display sidebar and expand
        objects$loaded <- TRUE

        # dashboardSidebar


        # if (btn_type == 'load') {
        #  objects$selected_row <- selected_row
        #  objects$loaded <- TRUE
        # }

      }

    })




#
#       Date_UOA_Species_DF <- make_metadata_df(Date_UOA_Species_List)
#
#
#       print('two')
#       Rel_Pol_DF <<- Rel_Pol_List %>%
#         reactiveValuesToList() %>%
#         as.data.frame()
#       print('three')
#       Metadata <- dplyr::left_join(Date_UOA_Species_DF, Rel_Pol_DF)
#
#       CU_DF <<- CU_List %>%
#         reactiveValuesToList() %>%
#         as.data.frame()
#
#       Metadata <- dplyr::left_join(Metadata, CU_DF)
#
#       WS_DF <<- WS_List %>%
#         reactiveValuesToList() %>%
#         as.data.frame()
#
#       Metadata <- dplyr::left_join(Metadata, WS_DF)
#       OUT <<- Metadata
#       Metadata
#     })





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
