selectize_colors <- c('#dbdbdb', '#00af50', '#ffcc00', '#ff742e', '#ff0000')

select_color_css <- function(id, input_val, selectize_colors, class='selectize-input', style=NULL) {
  input_val <- as.numeric(input_val)
  color <- selectize_colors[input_val]
  paste0('.', id, ' .', class, ' { background-color: ', color, ' !important;', style, '}')
}

score_categories <- list('Low', 'Moderate', 'Medium', 'High', 'Very High')


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  objects <- reactiveValues(info=NULL,
                            user_auth=FALSE,
                            loaded=TRUE)

  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash,
    sodium_hashed = TRUE,
    cookie_logins = FALSE,
    sessionid_col = sessionid,
    # cookie_getter = get_sessions_from_db
    # cookie_setter = add_session_to_db,
    log_out = reactive(logout_init())
  )


  observeEvent(credentials(), {
    if (credentials()$user_auth) {
      updateControlbar("controlbar")
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

  output$logininfo <- renderUI({
    shinyauthr::loginUI("login",
                        title='Login',
                        cookie_expiry = 7,
                        additional_ui = tagList(
                          HTML(knitr::kable(user_base[, c(1,2,4)],
                                            format = "html",
                                            table.attr = "style='width:60%;'"))
                        )
                        )
  })







  shinyhelper::observe_helpers(help_dir=file.path(app_sys(), 'app/helpfiles'))

  mod_sidebar_main_server("sidebar_main_1", objects)
  mod_home_server('home', objects)

  mod_life_stage_tabset_server('egg_alevin', 'Egg / Alevin')
  mod_life_stage_tabset_server('fry_parr', 'Fry / Parr')

  waiter::waiter_hide()
}








