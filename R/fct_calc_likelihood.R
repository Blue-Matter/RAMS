library(dplyr)
exposure_matrix <- read.csv('exposure_matrix.csv') %>% as.data.frame()
future_risk_matrix <- read.csv('future_risk_matrix.csv') %>% as.data.frame()

#' calc_likelihood
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
calc_likelihood <- function(spatial_scale, temporal_scale) {
  if (!is.null(spatial_scale) & !is.null(temporal_scale)) {
    val <- exposure_matrix %>%
      dplyr::filter(Spatial==spatial_scale, Temporal==temporal_scale) %>%
      dplyr::pull(Exposure)
  } else {
    val <- NULL
  }
  val

}

#' calc_future_score
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
calc_future_score <- function(risk_score, future_trend) {
  if (!is.null(risk_score) & !is.null(future_trend)) {
    val <- future_risk_matrix %>%
      dplyr::filter(Risk==risk_score, Future_Trend==future_trend) %>%
      dplyr::pull(Future_Risk)
  } else {
    val <- NULL
  }
  val

}



