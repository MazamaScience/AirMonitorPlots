#' @title Create timeseries plot for one or more monitors
#'
#' @description
#' This function wraps \link{tidy_ggDailyByHour}, accepting a `ws_monitor` 
#' object to assemble various layers to create a production-ready
#' timeseries plot for one or more monitors. 
#'
#' @inheritParams tidy_ggDailyByHour
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
#' monitor_ggDailyByHour(ws_monitor, monitorID = "410432002_01")

monitor_ggDailyByHour <- function(ws_monitor,
                                  startdate = NULL,
                                  enddate = NULL,
                                  monitorID = NULL,
                                  style = "large",
                                  title = NULL,
                                  timezone = NULL,
                                  ...) {
  
  if ( monitor_isMonitor(ws_monitor) ) {
    ws_tidy <- monitor_toTidy(ws_monitor)
  } else {
    stop("ws_monitor is not a ws_monitor object.")
  }
  
  tidy_ggDailyByHour(ws_tidy = ws_tidy,
                     startdate = startdate,
                     enddate = enddate,
                     monitorID = monitorID,
                     style = style,
                     title = title,
                     timezone = timezone,
                     ...)
  
}