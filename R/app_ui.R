

# How many days should sessions last?
cookie_expiry <- 7

user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  password_hash = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "facilitator"),
  name = c("User One", "User Two")
)



#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr %>%
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinydashboardPlus::dashboardPage(

      shinydashboardPlus::dashboardHeader(title='RAMS',
                                          controlbarIcon = shiny::icon("right-to-bracket"),
                                          tags$li(
                                            class = "dropdown",
                                            style = "padding: 8px;",
                                            shinyauthr::logoutUI("logout")
                                          )),
      controlbar=shinydashboardPlus::dashboardControlbar(id='controlbar',
                                                         width=350,
                                                         shinydashboardPlus::controlbarMenu(
                                                           id='menu',
                                                           shinydashboardPlus::controlbarItem(
                                                             title='',
                                                             uiOutput('logininfo')
                                                             )
                                                           )
                                                         ),

      shinydashboardPlus::dashboardSidebar(collapsed = TRUE,
                                           mod_sidebar_main_ui("sidebar_main_1")
                                           ),

      shinydashboard::dashboardBody(
        waiter::waiterShowOnLoad(), # will show on load

        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "home", mod_home_ui('home')),
          shinydashboard::tabItem(tabName = "egg_alevin", mod_life_stage_tabset_ui('egg_alevin')),
          shinydashboard::tabItem(tabName = "fry_parr", mod_life_stage_tabset_ui('fry_parr'))
      )
      )
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    tags$style(".table{margin: 0 auto;}"),

    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RAMS"
    ),


    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    waiter::use_waiter()


  )
}