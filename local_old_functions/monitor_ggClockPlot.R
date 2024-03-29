#' @title Create a clock plot for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready "clock"
#' plot for one monitor.
#'
#' @inheritParams ggplot_pm25Diurnal
#' @param mts_monitor A \emph{mts_monitor} object.
#' @param deviceDeploymentID deviceDeploymentID to include in the plot.
#' @param timezone Timezone for x-axis scale. If NULL and only one timezone
#'   present in the data, the data timezone will be used. If NULL and multiple
#'   timezones present, the default is UTC.
#'
#' @return A \emph{ggplot} object
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' mts_monitor <- AirMonitor::Carmel_Valley
#' monitor_ggClockPlot(mts_monitor, startdate = 20160801, enddate = 20160810)
#'
#' \dontrun{
#' mts_monitor <- airnow_loadLatest()
#' monitor_ggClockPlot(mts_monitor, deviceDeploymentID = "8e3f0e62b2af8cd7_410432002")
#' }
#'
monitor_ggClockPlot <- function(
  mts_monitor,
  startdate = NULL,
  enddate = NULL,
  deviceDeploymentID = NULL,
  timezone = NULL,
  ...
) {

  # Validate parameters --------------------------------------------------------

  if ( AirMonitor::monitor_isValid(mts_monitor) ) {
    mts_tidy <- monitor_toTidy(mts_monitor)
  } else {
    stop("mts_monitor must be a `mts_monitor` object.")
  }

  # Determine the timezone (code borrowed from custom_pm25TimeseriesScales.R)
  if ( is.null(timezone) ) {
    if ( length(unique(mts_tidy$timezone)) > 1 ) {
      timezone <- "UTC"
    } else {
      timezone <- mts_tidy$timezone[1]
    }
  }
  # Check timezone
  if ( !is.null(timezone) ) {
    if ( !timezone %in% OlsonNames() ) {
      stop("Invalid timezone")
    }
  }

  if ( length(unique(mts_tidy$deviceDeploymentID)) > 1 & is.null(deviceDeploymentID) )
    stop("deviceDeploymentID must be specified.")

  if (any(!deviceDeploymentID %in% unique(mts_tidy$deviceDeploymentID))) {
    invalidIDs <- deviceDeploymentID[which(!deviceDeploymentID %in% unique(mts_tidy$deviceDeploymentID))]
    stop(paste0("deviceDeploymentID not present in data: ", paste0(invalidIDs, collapse = ", ")))
  }

  if ( !is.null(startdate) & !is.null(enddate) ) {
    daterange <- range(mts_tidy$datetime)
    if (MazamaCoreUtils::parseDatetime(startdate, timezone = timezone) > daterange[2]) {
      stop("startdate is outside of data date range")
    }
    if (MazamaCoreUtils::parseDatetime(enddate, timezone = timezone) < daterange[1]) {
      stop("enddate is outside of data date range")
    }
  }

  # Prepare data ---------------------------------------------------------------

  if (!is.null(deviceDeploymentID)) {
    mts_tidy <- dplyr::filter(.data = mts_tidy, .data$deviceDeploymentID == !!deviceDeploymentID)
  }

  if (is.null(timezone)) {
    timezone <- unique(mts_tidy$timezone)
  }

  # Subset based on startdate and enddate
  if (!is.null(startdate)) {
    s <- MazamaCoreUtils::parseDatetime(startdate, timezone = timezone)
    mts_tidy <- dplyr::filter(mts_tidy, .data$datetime >= lubridate::floor_date(s, unit = "day"))
  } else {
    startdate <- min(mts_tidy$datetime)
  }

  if (!is.null(enddate)) {
    e <- MazamaCoreUtils::parseDatetime(enddate, timezone = timezone)
    mts_tidy <- dplyr::filter(mts_tidy, .data$datetime <= lubridate::ceiling_date(e, unit = "day"))
  } else {
    enddate <- max(mts_tidy$datetime)
  }

  # Create plot ----------------------------------------------------------------

  ## NOTE:
  #  Expand the xlim by 1/23 to add space for one additional bar. This is
  #  necessary to make room for the 11-12pm bar because the x-limit goes from
  #  0 to 23.

  plot <- ggplot_pm25Diurnal(
    mts_tidy,
    startdate = startdate,
    enddate = enddate,
    offsetBreaks = TRUE,
    xexp = c(1 / 23, 1 / 23),
    yexp = c(0.5, 0)
  )

  scales <- layer_scales(plot)
  ylim <- scales$y$limits

  plot <- plot +
    coord_polar(start = pi) +
    stat_meanByHour(
      aes(y = 0.8 * ylim[2]),
      input = "pm25",
      output = "AQIColors",
      width = 1
    ) +
    theme_clockPlot_airfire()

  # Add circle in the middle
  #
  # plot <- plot +
  #   annotate("rect",
  #     xmin = scales$x$limits[1], xmax = scales$x$limits[2],
  #     ymin = scales$y$limits[1], ymax = 0,
  #     fill = "black", color = NA, alpha = 0.5
  #   ) +
  #
  #   # This will add the keyhole on the bottom when there is space between the
  #   # end of hour 23 and the beginning of hour 0.
  #   # Edit the "xexp" parameter in ggplot_pm25Diurnal to make room for it.
  #
  #   annotate("rect",
  #     xmin = scales$x$limits[1], xmax = -0.5,
  #     ymin = 0, ymax = 1,
  #     color = NA, fill = "black", alpha =  0.5
  #   ) +
  #   annotate("rect",
  #     xmin = 23.5, xmax = scales$x$limits[2],
  #     ymin = 0, ymax = 1,
  #     color = NA, fill = "black", alpha = 0.5
  #   )

  return(plot)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  mts_monitor <- AirMonitor::monitor_loadLatest()
  startdate <- NULL
  enddate <- NULL
  deviceDeploymentID = "8e3f0e62b2af8cd7_410432002"
  timezone = NULL



}
