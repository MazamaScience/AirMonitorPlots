#' @title Create timeseries plot for one or more monitors
#'
#' @description
#' This function wraps \link{tidy_ggDailyBarplot}, accepting a `ws_monitor` 
#' object to assemble various layers to create a production-ready
#' timeseries plot for one or more monitors. 
#'
#' @inheritParams tidy_ggDailyBarplot
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
#' monitor_ggDailyBarplot(ws_monitor, monitorIDs = "410432002_01")

monitor_ggDailyBarplot <- function(ws_monitor,
                                   startdate = NULL,
                                   enddate = NULL,
                                   monitorIDs = NULL,
                                   style = "large", 
                                   title = NULL,
                                   timezone = NULL,
                                   today = TRUE) {
  
  if ( monitor_isMonitor(ws_monitor) ) {
    ws_tidy <- monitor_toTidy(ws_monitor)
  } else {
    stop("ws_monitor is not a ws_monitor object.")
  }
  
  tidy_ggDailyBarplot(ws_tidy,
                      startdate = startdate, 
                      enddate = enddate,
                      monitorIDs = monitorIDs,
                      style = style,
                      title = title,
                      timezone = timezone,
                      today = today)
  
}