

library(shinydashboard)
library(shinydashboardPlus)


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
#' @import shinydashboard
#' @import shinydashboardPlus
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
                                                         controlbarMenu(
                                                           id='menu',
                                                           controlbarItem(
                                                             title='',
                                                             uiOutput('logininfo')
                                                             )
                                                           )
                                                         ),

      shinydashboardPlus::dashboardSidebar(collapsed = TRUE,
                                           mod_sidebar_main_ui("sidebar_main_1")
                                           ),
        # shinydashboard::sidebarMenu(
        #   shinydashboard::menuItem('Home', tabName='home', icon=icon('home')),
        #
        #   # menuItem('Summary', tabName = 'summary', icon=icon('list')),
        #   menuItem('Egg / Alevin', tabName = 'egg_alevin', icon=icon('egg', class='fa-sm')),
        #   menuItem('Fry / Parr', tabName='fry_parr', icon=icon('fish', class='fa-xs')),
        #   menuItem('Smolt', tabName='smolt', icon=icon('fish', class='fa-sm')),
        #   menuItem('Juvenile', tabName='juvenile', icon=icon('fish')),
        #   menuItem('Immature', tabName='immature', icon=icon('fish', class='fa-lg')),
        #   menuItem('Adult', startExpanded = TRUE, icon=icon('fish', class='fa-xl'),
        #            menuSubItem('Return Migration', tabName='return_migration'),
        #            menuSubItem('Terminal Migration', tabName='terminal_migration'),
        #            menuSubItem('Spawning', tabName='spawning')
        #   )
        # )
      dashboardBody(
        waiter::waiterShowOnLoad(), # will show on load

        tabItems(
          tabItem(tabName = "home", mod_home_ui('home')),
          tabItem(tabName = "egg_alevin", mod_life_stage_tabset_ui('egg_alevin')),
          tabItem(tabName = "fry_parr", mod_life_stage_tabset_ui('fry_parr'))
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
