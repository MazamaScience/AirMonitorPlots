#' @title Instantiate a pm25 diurnal ggplot
#'
#' @description
#' Create a plot using ggplot with default mappings and styling. Layers can then
#' be added to this plot using \code{ggplot2} syntax.
#'
#' @inheritParams custom_pm25DiurnalScales
#'
#' @param mts_monitor Default dataset to use when adding layers. Must be either a
#'   \emph{mts_monitor} object or \code{mts_tidy} object.
#' @param startdate Desired startdate for data to include, in a format that can
#'   be parsed with \link{parseDatetime}.
#' @param enddate Desired enddate for data to include, in a format that can be
#'   parsed with \link{parseDatetime}.
#' @param timezone Timezone to use to set hours of the day
#' @param shadedNight add nighttime shading based on of middle day in selected
#'   period
#' @param mapping Default mapping for the plot
#' @param base_size Base font size for theme
#' @param ... Additional arguments passed on to
#'   \code{\link{custom_pm25DiurnalScales}}.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' mts_monitor <- AirMonitor::Carmel_Valley
#' ggplot_pm25Diurnal(mts_monitor) +
#'   coord_polar() +
#'   geom_pm25Points() +
#'   custom_aqiStackedBar(width = 1, alpha = .3)
#'
#' ggplot_pm25Diurnal(
#'   mts_monitor,
#'   startdate = 20160801,
#'   enddate = 20160810
#' ) +
#'   stat_boxplot(aes(group = hour))
#'
ggplot_pm25Diurnal <- function(
  mts_monitor,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  ylim = NULL,
  shadedNight = TRUE,
  mapping = aes_(x = ~hour, y = ~pm25),
  base_size = 11,
  ...
) {

  # ----- Validate Parameters --------------------------------------------------

  if ( !is.logical(shadedNight) )
    stop("shadedNight must be logical")

  if ( !is.numeric(base_size) )
    stop("base_size must be numeric")

  if ( AirMonitor::monitor_isValid(mts_monitor) ) {
    mts_tidy <- monitor_toTidy(mts_monitor)
  } else if ( monitor_isTidy(mts_monitor) ) {
    mts_tidy <- mts_monitor
  } else {
    stop("mts_monitor must be either a mts_monitor object or mts_tidy object.")
  }

  # Determine the timezone (code borrowed from custom_pm25TimeseriesScales.R)
  if ( is.null(timezone) ) {
    if ( length(unique(mts_tidy$timezone) ) > 1) {
      timezone <- "UTC"
      xlab <- "Time of Day (UTC)"
    } else {
      timezone <- mts_tidy$timezone[1]
      xlab <- "Time of Day (Local)"
    }
    # TODO:  This test for xlab fails because xlab is a function!
  } else if ( is.null(xlab) ) {
    xlab <- paste0("Time of Day (", timezone, ")")
  }

  if ( !is.null(startdate) ) {
    startdate <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
    if ( startdate > range(mts_tidy$datetime)[2] ) {
      stop("startdate is outside of data date range")
    }
  } else {
    startdate <- range(mts_tidy$datetime)[1]
  }

  if ( !is.null(enddate) ) {
    enddate <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)
    if ( enddate < range(mts_tidy$datetime)[1] ) {
      stop("enddate is outside of data date range")
    }
  } else {
    enddate <- range(mts_tidy$datetime)[2]
  }

  # ----- Prepare data ---------------------------------------------------------

  # MazamaCoreUtils::dateRange() was built for this!
  dateRange <- MazamaCoreUtils::dateRange(
    startdate,
    enddate,
    timezone = timezone,
    ceilingEnd = TRUE
  )
  startdate <- dateRange[1]
  enddate <- dateRange[2]

  # Subset based on startdate and enddate
  mts_tidy <- mts_tidy %>%
    dplyr::filter(.data$datetime >= startdate) %>%
    dplyr::filter(.data$datetime <= enddate)

  # Add column for 'hour'
  mts_tidy$hour <- as.numeric(strftime(mts_tidy$datetime, "%H", tz = timezone))
  mts_tidy$day  <- strftime(mts_tidy$datetime, "%Y%m%d", tz = timezone)

  # ----- Create plot ----------------------------------------------------------

  gg <- ggplot(mts_tidy, mapping) +
    theme_airfire(base_size = base_size) +
    custom_pm25DiurnalScales(mts_tidy, xlab = xlab, ylim = ylim, ...)

  # Calculate day/night shading
  if (shadedNight) {
    # Get the sunrise/sunset information
    ti <- MazamaTimeSeries::timeInfo(
      mts_tidy$datetime,
      longitude = mts_tidy$longitude[1],
      latitude = mts_tidy$latitude[1],
      timezone = mts_tidy$timezone[1]
    )

    # Extract the middle row
    ti <- ti[round(nrow(ti) / 2), ]

    # Get sunrise and sunset in units of hours
    sunrise <- lubridate::hour(ti$sunrise) + (lubridate::minute(ti$sunrise) / 60)
    sunset <- lubridate::hour(ti$sunset) + (lubridate::minute(ti$sunset) / 60)

    # Add shaded night
    scales <- layer_scales(gg)

    morning <- annotate(
      "rect",
      xmin = scales$x$limits[1],
      xmax = sunrise,
      ymin = scales$y$limits[1],
      ymax = scales$y$limits[2],
      fill = "black",
      alpha = 0.1
    )
    night <-   annotate(
      "rect",
      xmin = sunset,
      xmax = scales$x$limits[2],
      ymin = scales$y$limits[1],
      ymax = scales$y$limits[2],
      fill = "black",
      alpha = 0.1
    )

    gg <- gg + morning + night
  }

  # ----- Return ---------------------------------------------------------------

  return(gg)

}
