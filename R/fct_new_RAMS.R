#' #' new_RAMS
#' #'
#' #' @description A fct function
#' #'
#' #' @return The return value, if any, from executing the function.
#' #'
#' #' @noRd
#'
#'
#' options(groups='ramsmysql',
#'         default.file='G:/Shared drives/BM shared/1. Projects/DFO_RAMS/app_config/.my.cnf')
#'
#' 'G:/Shared drives/BM shared/1. Projects/DFO_RAMS/app_config/.my.cnf'
#'
#'
#' library(DBI)
#'
#' con <- dbConnect(RMariaDB::MariaDB(), groups='ramsmysql',
#'                  default.file='G:/Shared drives/BM shared/1. Projects/DFO_RAMS/app_config/.my.cnf')
#'
#'
#' # USERS TABLE -----
#' check_unique_email <- function(email, USERS) {
#'   email %in% USERS$EMAIL
#' }
#'
#' connect_db <- function() {
#'   dbConnect(RMariaDB::MariaDB(), groups=options()$groups,
#'             default.file=options()$default.file)
#' }
#'
#' create_new_USER <- function(first_name, last_name, email, role) {
#'
#'   #TODO sql santize
#'   # TODO helpers for checking unique email
#'   con <- connect_db()
#'
#'   USERS <- dbReadTable(con, "USERS")
#'   on.exit(dbDisconnect(con))
#'
#'   new_user <- data.frame(matrix(NA, nrow=1, ncol=length(names(USERS))))
#'   names(new_user) <- names(USERS)
#'
#'   new_user$FIRST_NAME <- first_name
#'   new_user$LAST_NAME <- last_name
#'   new_user$EMAIL <- email
#'   new_user$ROLE <- role
#'   new_user$DATE_CREATED <- Sys.Date()
#'
#'   if (check_unique_email(email, USERS)) {
#'     stop('Email already exists')
#'   }
#'
#'   dbAppendTable(con, 'USERS', new_user)
#'
#' }
#'
#' new_user <- create_new_USER('Adrian', 'Hordyk', 'adrian@bluematterscience.com', 'admin')
#'
#'
#' # LIMITING_FACTORS TABLE -----
#'
#' LF <- read.csv('LIMITING_FACTORS.csv')
#'
#' # MASTER TABLE ----
#'
#'
#' # RAMS TABLE ----
#'
#' # DBI::dbWriteTable(con, 'LIMITING_FACTORS', LF)
#'
#' meta_data <- data.frame(RAMSid=NULL,
#'                         UOA=NULL,
#'                         Year=NULL,
#'                         RL=NULL,
#'                         CU=NULL,
#'                         WP=NULL,
#'                         idUSERS=NULL)
#'
#' dbWriteTable(con, 'METADATA', meta_data)
#'
#' user_id <- 4
#'
#' create_new_metadata <- function(user_id) {
#'
#'   con <- connect_db()
#'   user_info <- dbReadTable(con, "MASTER")
#'
#'
#'
#'   on.exit(dbDisconnect(con))
#'
#'
#'
#'
#' }
