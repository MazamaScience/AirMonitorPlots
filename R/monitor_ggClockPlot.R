#' @title Create a clock plot for one or more monitors
#'
#' @description
#' This function wraps \link{tidy_ggClockPlot}, accepting a \link{ws_monitor}
#' object to assemble various layers to create a production-ready
#' "clock" plot for one or more monitors.
#'
#' @inheritParams tidy_ggClockPlot
#' @param ws_monitor A \link{ws_monitor} object
#' @return A \emph{ggplot} object
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
#' monitor_ggClockPlot(ws_monitor, startdate = 20160801, enddate = 20160810)
monitor_ggClockPlot <- function(
  ws_monitor,
  startdate = NULL,
  enddate = NULL,
  monitorID = NULL,
  timezone = NULL,
  ...
) {

# Validate parameters ----------------------------------------------------------

  if (monitor_isMonitor(ws_monitor)) ws_tidy <- monitor_toTidy(ws_monitor)
  else stop("ws_monitor is not a ws_monitor object.")


# Pass to next function --------------------------------------------------------

  tidy_ggClockPlot(
    ws_tidy,
    startdate = startdate,
    enddate = enddate,
    monitorID = monitorID,
    timezone = timezone,
    ...
  )

}
