#' @title Create a timeseries plot for one or more monitors
#'
#' @description
#' This function wraps \link{tidy_ggTimeseries}, accepting a \code{ws_monitor}
#' object to assemble various layers to create a production-ready timeseries
#' plot for one or more monitors.
#'
#' @inheritParams tidy_ggTimeseries
#' @param ws_monitor A \code{ws_monitor} object
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
#' monitor_ggTimeseries(ws_monitor, monitorID = "410432002_01")
#' }
#'
#' ws_monitor <- PWFSLSmoke::Northwest_Megafires
#' monitor_ggTimeseries(
#'   ws_monitor,
#'   startdate = 20150815,
#'   enddate = 20150831,
#'   monitorIDs = "160690014_01"
#')
monitor_ggTimeseries <- function(
  ws_monitor,
  startdate = NULL,
  enddate = NULL,
  style = "small",
  aqiStyle = NULL,
  monitorIDs = NULL,
  title = NULL
) {

  if (monitor_isMonitor(ws_monitor)) ws_tidy <- monitor_toTidy(ws_monitor)
  else stop("ws_monitor is not a ws_monitor object.")

  tidy_ggTimeseries(
    ws_tidy,
    startdate = startdate,
    enddate = enddate,
    style = style,
    aqiStyle = aqiStyle,
    monitorIDs = monitorIDs,
    title = title
  )

}
