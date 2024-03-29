

selectize_colors <- c('#dbdbdb', '#00af50', '#ffcc00', '#ff742e', '#ff0000')

select_color_css <- function(parent_id, id, input_val, selectize_colors, class='selectize-input', style=NULL) {
  input_val <- as.numeric(input_val)
  color <- selectize_colors[input_val]
  paste0('.', parent_id, '.', id, ' .', class, ' { background-color: ', color, ' !important;', style, '}')
}

score_categories <- list('Low', 'Moderate', 'Medium', 'High', 'Very High')


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  waitress <- waiter::Waitress$new(theme = "overlay-percent") # call the waitress

  objects <- reactiveValues(info=NULL,
                            user_auth=FALSE,
                            loaded=FALSE)


  sessionlist <- reactiveValues(list=NULL)

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = RAMS::USERS,
    user_col = idUSER,
    pwd_col = PASSWORD,
    sodium_hashed = TRUE,
    cookie_logins = FALSE,
    sessionid_col = sessionid,
    # cookie_getter = get_sessions_from_db
    # cookie_setter = add_session_to_db,
    log_out = reactive(logout_init())
  )


  observeEvent(credentials(), {
    if (credentials()$user_auth) {
      shinydashboardPlus::updateControlbar("controlbar")
      objects$info <- credentials()$info
      objects$user_auth <- TRUE
    } else {
      objects$info <- NULL
      objects$user_auth <- FALSE
    }
  })

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  output$user <- renderUI({
    if (credentials()$user_auth) {
      tagList(
        h5(objects$info$PERMISSIONS, style="color: #fff;")
      )
    } else {
      NULL
    }
  })

  output$logininfo <- renderUI({
    shinyauthr::loginUI("login",
                        title='Login',
                        cookie_expiry = 7,
                        additional_ui = tagList(
                          br(),
                          p('Login with your RAMS user name and password.'),
                          p('Please contact the', a(href='mailto:example@email.com','RAMS App administrator'),
                            'to register for a RAMs user name and password'),
                          br(),
                          p('Development login details:'),
                          HTML(knitr::kable(data.frame(User=RAMS::USERS$idUSER,
                                                       Password=c('pass1', 'pass2'),
                                                       Role=RAMS::USERS$PERMISSIONS),
                                            format = "html",
                                            table.attr = "style='width:100%;'")
                          )
                        )


    )
  })

  shinyjs::delay(30,
                 shinyjs::show('login-panel'))

  waitress$set(5)

  shinyhelper::observe_helpers(help_dir=file.path(app_sys(), 'app/helpfiles'))

  mod_sidebar_main_server("sidebar_main_1", objects)

  waitress$inc(10)
  mod_home_server('home', objects, credentials, home_session=session)
  mod_summary_server("summary", objects, home_session)
  mod_table_server("table", objects, home_session)

  waitress$inc(10)
  mod_life_stage_tabset_server('egg_alevin', 'Egg / Alevin', 'Freshwater Egg Incubation', objects, home_session, icon=icon('egg', class='fa-sm'))
  waitress$inc(10)
  mod_life_stage_tabset_server('fry_parr', 'Fry / Parr', 'Freshwater Fry Rearing', objects,home_session, icon=icon('fish', class='fa-xs'))
  waitress$inc(10)
  mod_life_stage_tabset_server('smolt', 'Smolt', 'Estuarine Smolt Migration and Rearing', objects, home_session, icon=icon('fish', class='fa-sm'))
  waitress$inc(10)
  mod_life_stage_tabset_server('juvenile', 'Juvenile', 'Marine Nearshore Juvenile Rearing and Migration', objects, home_session, icon=icon('fish', class='fa-xs'))
  waitress$inc(10)
  mod_life_stage_tabset_server('immature', 'Immature', 'Marine Pelagic Immature Rearing', objects, home_session,icon=icon('fish'))
  waitress$inc(10)
  mod_life_stage_tabset_server('return_migration', 'Adult', 'Marine Adult Return Migration', objects, home_session, icon=icon('fish', class='fa-lg'))
  waitress$inc(10)
  mod_life_stage_tabset_server('terminal_migration', 'Adult', 'Estuary and Freshwater Adult Terminal Migration', objects, home_session, icon=icon('fish', class='fa-lg'))
  waitress$inc(10)
  mod_life_stage_tabset_server('spawning', 'Adult', 'Freshwater Adult Spawning', objects, home_session,icon=icon('fish', class='fa-lg'))

  waitress$close()
}








