#' @title Create timeseries plot for one or more monitors
#'
#' @description
#' This function wraps \link{tidy_ggTimeseries}, accepting a `ws_monitor` 
#' object to assemble various layers to create a production-ready
#' timeseries plot for one or more monitors. 
#'
#' @inheritParams tidy_ggTimeseries
#' @param ws_monitor A `ws_monitor` object
#' @return A **ggplot** object
#'
#' @import ggplot2
#' @importFrom rlang .data
#' 
#' @export
#' 
#' @examples 
#' ws_monitor <- airnow_loadLatest()
#' monitor_ggTimeseries(ws_monitor, monitorIDs = "410432002_01")

monitor_ggTimeseries <- function(ws_monitor,
                                 startdate = NULL,
                                 enddate = NULL,
                                 style = NULL,
                                 aqiStyle = NULL,
                                 monitorIDs = NULL,
                                 title = NULL) {
  
  if ( monitor_isMonitor(ws_monitor) ) {
    ws_tidy <- monitor_toTidy(ws_monitor)
  } else {
    stop("ws_monitor is not a ws_monitor object.")
  }
  
  tidy_ggTimeseries(ws_tidy,
                    startdate = startdate, 
                    enddate = enddate,
                    style = style, 
                    aqiStyle = aqiStyle,
                    monitorIDs = monitorIDs,
                    title = title)
  
}