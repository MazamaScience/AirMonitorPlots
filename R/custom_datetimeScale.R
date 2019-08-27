#' @title Add date scale with custom styling
#'
#' @description
#' Add a date scale and custom formatting for creating
#' consistent timeseries plots.
#'
#' @param startdate Desired axis start date limit, in a format that can be
#'   parsed with \link{parseDatetime}.
#' @param enddate Desired axis end date limit, in a format that can be parsed
#'   with \link{parseDatetime}.
#' @param timezone Timezone for label formatting.
#' @param expand Vector of range expansion constants used to add some padding
#'   around the data, to ensure that they are placed some distance away from the
#'   axes.
#' @param break_width Space between breaks. If \code{NULL}, suitable breaks are
#'   calculated based on the data. See 'Details'.
#' @param minor_break_width Space between minor breaks. If \code{NULL}, suitable
#'   breaks are calculated based on the data. See 'Details'.
#' @param date_labels date format string for formatting date labels.
#' @param tick_location Location of ticks ("midnight" or "midday")
#' @param includeFullEnddate if \code{TRUE}, the x-axis limit is pushed up to
#'   include the full final day.
#' @param today_label if \code{FALSE}, no label will be generated for today.
#' @param ... Additional arguments passed onto
#'   \code{\link[ggplot2]{scale_x_datetime}}.
#'
#' @details
#' \code{break_width} and \code{minor_break_width} can be specified in several
#' ways.
#' \itemize{
#'   \item A number, taken to be in days
#'   \item An object of class \code{difftime}
#'   \item A character string, containing one of \code{"day"}, \code{"week"},
#'         \code{"month"}, \code{"quarter"} or \code{"year"}. This can
#'         optionally be preceded by a (positive or negative) integer and a
#'         space or followed by \code{"s"}. For example: \code{"3 weeks"}.
#' }
#'
#' @export
#' @import ggplot2
custom_datetimeScale <- function(
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  expand = c(0, 0.05),
  break_width = NULL,
  minor_break_width = NULL,
  date_labels = "%b %d",
  tick_location = c("midnight", "midday")[1],
  includeFullEnddate = TRUE,
  today_label = TRUE,
  ...
) {

  # Validate parameters --------------------------------------------------------

  if (is.null(startdate)) stop("startdate must be specified")
  if (is.null(enddate)) stop("enddate must be specified")
  if (!is.null(timezone) && !timezone %in% OlsonNames()) stop("Invalid timezone.")
  if (class(expand) != "numeric" || length(expand) != 2) stop("Invalid 'expand'.")
  if (!tick_location %in% c("midnight", "midday")) {
    stop("Invalid tick_location. Choose from 'midnight' or 'midday'")
  }
  if (!is.logical(includeFullEnddate)) stop("includeFullEnddate must be logical.")
  if (!is.logical(today_label)) stop("today_label must be logical.")


  # Handle start/end dates -----------------------------------------------------

  # TODO: can this all just be replaced with `parseDatetime`?

  # Handle various startdates
  if (!is.null(startdate)) {
    if (is.numeric(startdate) || is.character(startdate)) {
      startdate <- parseDatetime(startdate, timezone = timezone)
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
      enddate <- parseDatetime(enddate, timezone = timezone)
    } else if (lubridate::is.POSIXct(enddate)) {
      enddate <- lubridate::force_tz(enddate, tzone = timezone)
    } else if (!is.null(enddate)) {
      stop(paste0(
        "Required parameter 'enddate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }

  # We will include the complete 'enddate' day
  dayCount <- as.integer(difftime(enddate, startdate, units = "days")) + 1


  # Calculate major/minor breaks -----------------------------------------------

  if (tick_location == "midnight") {
    s <- lubridate::floor_date(startdate, unit = "day")
    e <- lubridate::ceiling_date(enddate, unit = "day") # full 24 hours of enddate
  } else if (tick_location == "midday") {
    s <- lubridate::floor_date(startdate) + lubridate::dhours(12)
    e <- lubridate::ceiling_date(enddate) + lubridate::dhours(12)
  }

  if (dayCount >= 0 && dayCount <= 11) {
    break_width <- ifelse(is.null(break_width), "1 day", break_width)
    minor_break_width <- ifelse(is.null(minor_break_width), "3 hours", minor_break_width)
    text_angle <- 0
    text_hjust <- 0.5
  } else if (dayCount <= 21) {
    break_width <- ifelse(is.null(break_width), "3 days", break_width)
    minor_break_width <- ifelse(is.null(minor_break_width), "6 hours", minor_break_width)
    text_angle <- 45
    text_hjust <- 1
  } else if (dayCount <= 60) {
    break_width <- ifelse(is.null(break_width), "1 week", break_width)
    minor_break_width <- ifelse(is.null(minor_break_width), "1 day", minor_break_width)
    text_angle <- 45
    text_hjust <- 1
  } else if (dayCount <= 120) {
    break_width <- ifelse(is.null(break_width), "2 weeks", break_width)
    minor_break_width <- ifelse(is.null(minor_break_width), "1 day", minor_break_width)
    text_angle <- 45
    text_hjust <- 1
  } else {
    break_width <- ifelse(is.null(break_width), "1 month", break_width)
    minor_break_width <- ifelse(is.null(minor_break_width), "3 week", minor_break_width)
    text_angle <- 45
    text_hjust <- 1
  }

  breaks <- seq(s, e, by = break_width)
  minor_breaks <- seq(s, e, by = minor_break_width)


  # Calculate labels -----------------------------------------------------------

  if (!today_label) {
    labels <- strftime(breaks, date_labels, tz = timezone)
    if (
      lubridate::floor_date(lubridate::now(tzone = timezone), "day") %in%
      lubridate::floor_date(breaks, "day")
    ) {
      labels[length(labels)] <- ""
    }
    date_labels <- waiver()
  } else {
    labels <- waiver()
  }


  # Calculate padding ----------------------------------------------------------

  # NOTE:  X-axis must be extended to fit the complete last day.
  # NOTE:  Then a little bit more for style.
  xRangeSecs <- as.numeric(difftime(enddate, startdate, timezone, units = "secs"))
  marginSecs <- 0.02 * xRangeSecs
  xlo <- startdate - lubridate::dseconds(marginSecs)
  if (includeFullEnddate) {
    xhi <-
      lubridate::floor_date(enddate, unit = "day") +
      lubridate::dhours(23) +
      lubridate::dseconds(marginSecs)
  } else {
    xhi <- enddate + lubridate::dseconds(marginSecs)
  }


  # Add x-axis -----------------------------------------------------------------

  list(
    scale_x_datetime(
      limits = c(xlo, xhi),
      expand = expand,
      breaks = breaks,
      minor_breaks = minor_breaks,
      date_labels = date_labels,
      timezone = timezone,
      labels = labels,
      ...
    ),
    theme(
      ###axis.ticks.x = element_line(),
      axis.text.x = element_text(
        angle = text_angle,
        hjust = text_hjust
      )
    )
  )

}
