#' @title AirFire PM2.5 timeseries scales
#'
#' @description
#' Add AirFire-style x-axis and y-axis scales suitable for a timeseries plot
#' showing PM2.5 data.
#'
#' @param monitor A \emph{mts_monitor} object.Should match the default dataset
#'   of the plot.
#' @param startdate Desired start date (integer or character in ymd format or
#'   POSIXct).
#' @param enddate Desired end date (integer or character in ymd format or
#'   POSIXct).
#' @param ylim custom y-axis limits. This function will apply a default limit
#'   depending on the data.
#' @param timezone Timezone for x-axis scale. If NULL and only one timezone
#'   present in \code{monitor}, that timezone will be used. If NULL and multiple
#'   timezones present, the default is UTC.
#' @param xlab Custom xlab. If \code{NULL} a default xlab will be generated.
#' @param yexp Vector of range expansion constants used to add some padding
#'   around the data on the y-axis, to ensure that they are placed some distance
#'   away from the axes.
#' @param xexp Vector of range expansion constants used to add some padding
#'   around the data on the x-axis, to ensure that they are placed some distance
#'   away from the axes.
#' @param ... Additional arguments passed on to
#'   \code{\link{custom_datetimeScale}}.
#'
#' @importFrom rlang .data
#' @import ggplot2
#' @export
custom_pm25TimeseriesScales <- function(
  monitor = NULL,
  startdate = NULL,
  enddate = NULL,
  ylim = NULL,
  timezone = NULL,
  xlab = NULL,
  yexp = c(0.05, 0),
  xexp = c(0, 0.05),
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  if (is.null(monitor)) {
    if (is.null(startdate) || is.null(enddate) || is.null(ylim)) {
      stop("at least one of monitor, startdate, enddate, and ylim must be specified.")
    }
  }

  # Convert to mts_tidy
  mts_tidy <- monitor_toTidy(monitor)

  if (is.null(startdate)) startdate <- min(mts_tidy$datetime)
  if (is.null(enddate)) enddate <- max(mts_tidy$datetime)

  if (is.null(timezone)) {
    if (length(unique(mts_tidy$timezone)) > 1) {
      timezone <- "UTC"
      xlab <- "Time (UTC)"
    } else {
      timezone <- mts_tidy$timezone[1]
      xlab <- "Local Time"
    }
  } else if (is.null(xlab)) {
    xlab <- paste0("Time (", timezone, ")")
  }


  # ----- Handle start/end dates -----------------------------------------------

  # TODO: can this all just be replaced with ``?

  # Handle various startdates
  if (!is.null(startdate)) {
    if (is.numeric(startdate) || is.character(startdate)) {
      startdate <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
    } else if (lubridate::is.POSIXct(startdate)) {
      startdate <- lubridate::force_tz(startdate, tzone = timezone)
    } else if (!is.null(startdate)) {
      stop(paste0(
        "Required parameter 'startdate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }

  # Handle various enddates
  if (!is.null(enddate)) {
    if (is.numeric(enddate) || is.character(enddate)) {
      enddate <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)
    } else if (lubridate::is.POSIXct(enddate)) {
      enddate <- lubridate::force_tz(enddate, tzone = timezone)
    } else if (!is.null(enddate)) {
      stop(paste0(
        "Required parameter 'enddate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }


  # ----- Calculate axis limits ------------------------------------------------

  # Default to well defined y-axis limits for visual stability
  if (is.null(ylim)) {
    ylo <- 0
    ymax <- mts_tidy %>%
      dplyr::filter(.data$datetime >= startdate & .data$datetime <= enddate) %>%
      magrittr::use_series("pm25") %>%
      max(na.rm = TRUE)

    yhi <- dplyr::case_when(
      ymax <= 50   ~ 50,
      ymax <= 100  ~ 100,
      ymax <= 200  ~ 200,
      ymax <= 400  ~ 400,
      ymax <= 600  ~ 600,
      ymax <= 1000 ~ 1000,
      ymax <= 1500 ~ 1500,
      TRUE         ~ 1.05 * ymax
    )

  } else {
    # Standard y-axis limits
    ylo <- ylim[1]
    yhi <- ylim[2]
  }


  # ----- Add scales -----------------------------------------------------------

  list(
    custom_datetimeScale(
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      expand = xexp,
      ...
    ),
    scale_y_continuous(
      limits = c(ylo, yhi),
      expand = yexp
    ),
    ylab("PM2.5 (\u00b5g/m3)"),
    xlab(xlab)
  )

}
