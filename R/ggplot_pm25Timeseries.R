#' @export
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @title Create a pm25 timeseries ggplot
#'
#' @description
#' Create a plot using ggplot with default mappings and styling. Layers can then
#' be added to this plot using \code{ggplot2} syntax.
#'
#' @inheritParams custom_pm25TimeseriesScales
#'
#' @param monitor Monitoring data object to use when adding layers. Must be of
#' class \code{mts_monitor} or \code{mts_tidy}.
#' @param base_size Base font size for theme.
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' AirMonitor::Carmel_Valley %>%
#'   ggplot_pm25Timeseries() +
#'   geom_point(shape = "square", alpha = .4)
#'
ggplot_pm25Timeseries <- function(
  monitor,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  ylim = NULL,
  base_size = 11,
  ...
) {

  # ----- Validate Parameters --------------------------------------------------

  if ( AirMonitor::monitor_isValid(monitor) ) {
    mts_tidy <- monitor_toTidy(monitor)
  } else if ( monitor_isTidy(monitor) ) {
    mts_tidy <- monitor
  } else {
    stop("monitor must be either a mts_monitor object or mts_tidy object.")
  }

  # Determine the timezone (code borrowed from custom_pm25TimeseriesScales.R)
  if ( is.null(timezone) ) {
    if ( length(unique(mts_tidy$timezone)) > 1 ) {
      timezone <- "UTC"
    } else {
      timezone <- mts_tidy$timezone[1]
    }
  }

  if ( !is.null(startdate) ) {
    startdate <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
    if ( startdate > range(mts_tidy$datetime)[2] ) {
      stop("startdate is outside of data date range")
    }
  }

  if ( !is.null(enddate) ) {
    enddate <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)
    if ( enddate < range(mts_tidy$datetime)[1] ) {
      stop("enddate is outside of data date range")
    }
  }

  if ( !is.numeric(base_size) )
    stop("base_size must be numeric")

  # ----- Create plot ----------------------------------------------------------

  gg <-
    ggplot(mts_tidy, aes_(x = ~datetime, y = ~pm25)) +
    theme_airfire(base_size) +
    custom_pm25TimeseriesScales(
      mts_tidy,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      ylim = ylim,
      ...
    )

  return(gg)

}
