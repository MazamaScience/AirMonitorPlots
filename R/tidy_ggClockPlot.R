#' @title Create a "clock" plot for a single monitor
#'
#' @description
#' This function assembles various layers to create a production-ready
#' "clock" plot for one monitor.
#'
#' @inheritParams ggplot_pm25Diurnal
#' @param ws_tidy dataframe of monitor data, created from a \code{ws_monitor}
#' object using \code{monitor_toTidy()}.
#' @param monitorID monitorID to include in the plot.
#' @param timezone Timezone for x-axis scale. If NULL and only one timezone present
#' in the data, the data timezone will be used. If NULL and multiple timezones
#' present, the default is UTC.
#' @return A **ggplot** object
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ws_monitor <- airnow_loadLatest()
#' ws_tidy <- monitor_toTidy(ws_monitor)
#' tidy_ggClockPlot(ws_tidy, monitorID = "060631010_01")
#' }
tidy_ggClockPlot <- function(
  ws_tidy,
  startdate = NULL,
  enddate = NULL,
  monitorID = NULL,
  timezone = NULL,
  ...
) {

  # Validate Parameters --------------------------------------------------------

  if (!monitor_isTidy(ws_tidy)) stop("ws_tidy must be a ws_tidy object")

  if (!is.null(timezone) && !timezone %in% OlsonNames())
    stop("Invalid timezone")

  if (length(unique(ws_tidy$monitorID)) > 1 & is.null(monitorID))
    stop("monitorID must be specified.")

  if (any(!monitorID %in% unique(ws_tidy$monitorID))) {
    invalidIDs <- monitorID[which(!monitorID %in% unique(ws_tidy$monitorID))]
    stop(paste0("monitorID not present in data: ", paste0(invalidIDs, collapse = ", ")))
  }

  if (!is.null(startdate) & !is.null(enddate)) {
    daterange <- range(ws_tidy$datetime)
    if (parseDatetime(startdate) > daterange[2]) {
      stop("startdate is outside of data date range")
    }
    if (parseDatetime(enddate) < daterange[1]) {
      stop("enddate is outside of data date range")
    }
  }

  # Prepare data ---------------------------------------------------------------

  if (!is.null(monitorID)) {
    ws_tidy <- dplyr::filter(.data = ws_tidy, .data$monitorID == !!monitorID)
  }

  if (is.null(timezone)) {
    timezone <- unique(ws_tidy$timezone)
  }

  # Subset based on startdate and enddate
  if (!is.null(startdate)) {
    s <- parseDatetime(startdate, timezone = timezone)
    ws_tidy <- dplyr::filter(ws_tidy, .data$datetime >= lubridate::floor_date(s, unit = "day"))
  } else {
    startdate <- min(ws_tidy$datetime)
  }

  if (!is.null(enddate)) {
    e <- parseDatetime(enddate, timezone = timezone)
    ws_tidy <- dplyr::filter(ws_tidy, .data$datetime <= lubridate::ceiling_date(e, unit = "day"))
  } else {
    enddate <- max(ws_tidy$datetime)
  }

  # Create plot ----------------------------------------------------------------

  ## NOTE:
  #  Expand the xlim by 1/23 to add space for one additional bar. This is
  #  necessary to make room for the 11-12pm bar because the x-limit goes from
  #  0 to 23.

  plot <- ggplot_pm25Diurnal(ws_tidy,
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
    stat_meanByHour(aes(y = 0.8 * ylim[2]),
                    input = "pm25",
                    output = "AQIColors",
                    width = 1) +
    theme_clockPlot_pwfsl()

  # Add circle in the middle
  #
  # plot <- plot + annotate("rect",
  #                 xmin = scales$x$limits[1],
  #                 xmax = scales$x$limits[2],
  #                 ymin = scales$y$limits[1],
  #                 ymax = 0,
  #                 fill = "black",
  #                 color = NA,
  #                 alpha = 0.5) +
  #   # This will add the keyhole on the bottom when there is space between the end of hour 23 and the beginning of hour 0.
  #   # Edit the "xexp" parameter in ggplot_pm25Diurnal to make room for it.
  #   annotate("rect", xmin = scales$x$limits[1], xmax = -0.5, ymin = 0, ymax = 1, color = NA, fill = "black", alpha=  0.5) +
  #   annotate("rect", xmin = 23.5, xmax = scales$x$limits[2], ymin = 0, ymax = 1, color = NA, fill = "black", alpha = 0.5)

  return(plot)

}
