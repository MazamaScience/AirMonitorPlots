#' @title Instantiate a pm25 timeseries ggplot
#'
#' @description
#' Create a plot using ggplot with default mappings and styling. Layers can then 
#' be added to this plot using \code{ggplot2} syntax. 
#'
#' @param ws_data Default dataset to use when adding layers. Must be either a \code{ws_monitor} 
#' object or \code{ws_tidy} object. 
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @example 
#' ws_monitor <- airsis_loadLatest()
#' ggplot_pm25Timeseries(ws_monitor) +
#'   geom_point(shape = "square", 
#'              alpha = .2) +
#'   custom_pm25TimeseriesScales()



ggplot_pm25Timeseries <- function(ws_data) {
  
  if ( "ws_monitor" %in% class(ws_data) ) {
    ws_tidy <- monitor_toTidy(ws_data)
  } else if ( "ws_tidy" %in% class(ws_data) ) {
    ws_tidy <- ws_data
  } else {
    stop("ws_data must be either a ws_monitor object or ws_tidy object.")
  }
  
  ggplot(ws_tidy, aes_(x = ~datetime, y = ~pm25)) +
    theme_timeseriesPlot_pwfsl()
  
}

