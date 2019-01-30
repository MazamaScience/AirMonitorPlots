#' @title Instantiate a pm25 timeseries ggplot
#'
#' @description
#' Create a plot using ggplot with default mappings and styling. Layers can then 
#' be added to this plot using \code{ggplot2} syntax. 
#'
#' @inheritParams custom_pm25TimeseriesScales
#' @param ws_data Default dataset to use when adding layers. Must be either a \code{ws_monitor} 
#' object or \code{ws_tidy} object. 
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' ggplot_pm25Timeseries(ws_monitor) +
#'   geom_point(shape = "square", 
#'              alpha = .2)



ggplot_pm25Timeseries <- function(ws_data,
                                  startdate = NULL,
                                  enddate = NULL,
                                  timezone = NULL,
                                  ylim = NULL,
                                  ...) {
  
  if ( monitor_isMonitor(ws_data) ) {
    ws_tidy <- monitor_toTidy(ws_data)
  } else if ( monitor_isTidy(ws_data) ) {
    ws_tidy <- ws_data
  } else {
    stop("ws_data must be either a ws_monitor object or ws_tidy object.")
  }
  
  ggplot(ws_tidy, aes_(x = ~datetime, y = ~pm25)) +
    theme_timeseriesPlot_pwfsl() +
    custom_pm25TimeseriesScales(ws_tidy, 
                                startdate = startdate, 
                                enddate = enddate, 
                                timezone = timezone, 
                                ylim = ylim,
                                ...)
  
}