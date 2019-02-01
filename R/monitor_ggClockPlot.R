#' @title Create clock plot for one monitor
#'
#' @description
#' This function wraps \link{tidy_ggDailyBarplot}, accepting a `ws_monitor` 
#' object to assemble various layers to create a production-ready
#' timeseries plot for one or more monitors. 
#'
#' @inheritParams tidy_ggClockPlot
#' @param ws_monitor A `ws_monitor` object
#' @return A **ggplot** object
#'
#' @import ggplot2
#' @importFrom rlang .data
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' ws_monitor <- airnow_loadLatest()
#' monitor_ggClockPlot(ws_monitor, monitorID = "410432002_01")
#' }
#' 
#' ws_monitor <- Carmel_Valley
#' monitor_ggClockPlot(ws_monitor, 
#'                     startdate = 20160801,
#'                     enddate = 20160810)
#' 


monitor_ggClockPlot <- function(ws_monitor,
                                startdate = NULL,
                                enddate = NULL,
                                monitorID = NULL,
                                timezone = NULL,
                                ...) {
  
  if ( monitor_isMonitor(ws_monitor) ) {
    ws_tidy <- monitor_toTidy(ws_monitor)
  } else {
    stop("ws_monitor is not a ws_monitor object.")
  }
  
  tidy_ggClockPlot(ws_tidy,
                   startdate = startdate,
                   enddate = enddate,
                   monitorID = monitorID,
                   timezone = timezone,
                   ...)
  
}