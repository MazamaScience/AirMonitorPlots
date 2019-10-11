#' @title Instantiate a pm25 timeseries ggplot
#'
#' @description
#' Create a plot using ggplot with default mappings and styling. Layers can then
#' be added to this plot using \code{ggplot2} syntax.
#'
#' @inheritParams custom_pm25TimeseriesScales
#' @param ws_data Default dataset to use when adding layers. Must be either a
#'   \code{ws_monitor} or \code{ws_tidy} object.
#' @param base_size Base font size for theme
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' ggplot_pm25Timeseries(ws_monitor) +
#'   geom_point(shape = "square",
#'              alpha = .2)
#'
ggplot_pm25Timeseries <- function(
  ws_data,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  ylim = NULL,
  base_size = 11,
  ...
) {

  # ----- Validate Parameters --------------------------------------------------

  if ( monitor_isMonitor(ws_data) ) {
    ws_tidy <- monitor_toTidy(ws_data)
  } else if ( monitor_isTidy(ws_data) ) {
    ws_tidy <- ws_data
  } else {
    stop("ws_data must be either a ws_monitor object or ws_tidy object.")
  }

  # Determine the timezone (code borrowed from custom_pm25TimeseriesScales.R)
  if ( is.null(timezone) ) {
    if ( length(unique(ws_tidy$timezone)) > 1 ) {
      timezone <- "UTC"
    } else {
      timezone <- ws_tidy$timezone[1]
     }
  }

  if ( !is.null(startdate) ) {
    startdate <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
    if ( startdate > range(ws_tidy$datetime)[2] ) {
      stop("startdate is outside of data date range")
    }
  }

  if ( !is.null(enddate) ) {
    enddate <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)
    if ( enddate < range(ws_tidy$datetime)[1] ) {
      stop("enddate is outside of data date range")
    }
  }

  if ( !is.numeric(base_size) )
    stop("base_size must be numeric")

  # ----- Create plot ----------------------------------------------------------

  gg <- ggplot(ws_tidy, aes_(x = ~datetime, y = ~pm25)) +
      theme_pwfsl(base_size) +
      custom_pm25TimeseriesScales(
        ws_tidy,
        startdate = startdate,
        enddate = enddate,
        timezone = timezone,
        ylim = ylim,
        ...
      )

  return(gg)

}
