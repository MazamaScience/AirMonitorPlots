#' @title Instantiate a pm25 timeseries ggplot
#'
#' @description
#' Create a plot using ggplot with default mappings and styling. Layers can then
#' be added to this plot using \code{ggplot2} syntax.
#'
#' @inheritParams custom_pm25TimeseriesScales
#' @param mts_monitor Default dataset to use when adding layers. Must be either a
#'   \code{mts_monitor} or \code{mts_tidy} object.
#' @param base_size Base font size for theme
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @examples
#' mts_monitor <- AirMonitor::Carmel_Valley
#' ggplot_pm25Timeseries(mts_monitor) +
#'   geom_point(shape = "square",
#'              alpha = .2)
#'
ggplot_pm25Timeseries <- function(
  mts_monitor,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  ylim = NULL,
  base_size = 11,
  ...
) {

  # ----- Validate Parameters --------------------------------------------------

  if ( monitor_isValid(mts_monitor) ) {
    mts_tidy <- monitor_toTidy(mts_monitor)
  } else if ( monitor_isTidy(mts_monitor) ) {
    mts_tidy <- mts_monitor
  } else {
    stop("mts_monitor must be either a mts_monitor object or mts_tidy object.")
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

  gg <- ggplot(mts_tidy, aes_(x = ~datetime, y = ~pm25)) +
      theme_pwfsl(base_size) +
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
