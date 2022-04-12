#' @title Create an archival timeseries plot for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' archival timeseries plot for one or more monitors.
#'
#' @param monitor A \emph{mts_monitor} object.
#' @param startdate Desired start date (integer or character in ymd format or
#'   POSIXct).
#' @param enddate Desired end date (integer or character in ymd format or
#'   POSIXct).
#' @param id vector of deviceDeploymentIDs to include in the plot. If more than
#'   one, different monitors will be plotted in different colors.
#' @param style Plot style. \code{small} or \code{large}. \code{style = small}
#'   is appropriate for plots 450x450px or smaller; \code{style = large} is
#'   appropriate for plots larger than 450x450px.
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param timezone Olson timezone name for x-axis scale and date parsing. If
#'   NULL the timezone of the specified monitor will be used.
#' @param ... Arguments passed onto \code{\link{ggplot_pm25Timeseries}}.
#'
#' @return A \emph{ggplot} object.
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' AirMonitor::NW_Megafires %>%
#'   monitor_ggTimeseries_archival(
#'     startdate = 20150809,
#'     enddate = 20150820,
#'     id = "575243c65b9e4719_160690012",
#'     timezone = "America/Los_Angeles"
#'  )
#'
#' AirMonitor::Carmel_Valley %>%
#'   AirMonitor::monitor_trimDate() %>%
#'   monitor_ggTimeseries_archival()
#'

# TODO:  These arguments should be harmonized with those from other plotting functions
monitor_ggTimeseries_archival <- function(
  monitor,
  startdate = NULL,
  enddate = NULL,
  id = NULL,
  style = c("large", "small"),
  title = NULL,
  timezone = NULL,
  ...
) {

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  # Convert monitor to tidy structure
  if ( AirMonitor::monitor_isValid(monitor) ) {
    mts_tidy <- monitor_toTidy(monitor)
  } else {
    stop("monitor is not a mts_monitor object.")
  }

  # Check style
  style <- match.arg(style)

  # Check deviceDeploymentIDs
  if ( any(!id %in% unique(mts_tidy$deviceDeploymentID)) ) {
    invalidIDs <- id[which(!id %in% unique(mts_tidy$deviceDeploymentID))]
    stop(paste0(
      "Invalid ids specified. 'monitor' does not contain deviceDeploymentIDs: ",
      paste0(invalidIDs, collapse = ", ")
    ))
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

  # ----- Prepare data ---------------------------------------------------------

  if ( !is.null(id) ) {
    mts_tidy <- dplyr::filter(mts_tidy, .data$deviceDeploymentID %in% id)
  }

  pm25LegendLabel <- "Hourly PM2.5 Values"
  nowcastLegendLabel <- "NowCast"

  if (length(unique(mts_tidy$deviceDeploymentID)) > 1) {
    mapping_pm25 <- mapping_nowcast <- aes_(color = ~deviceDeploymentID)
    if (is.null(title)) title <- ""
  } else {
    mapping_pm25 <- aes(color = !!pm25LegendLabel)
    mapping_nowcast <- aes(color = !!nowcastLegendLabel)
    if (is.null(title)) {
      title <- paste0("Hourly PM2.5 Values and NowCast\n",
                      "Site: ", unique(mts_tidy$locationName),
                      " (", unique(mts_tidy$deviceDeploymentID), ")")
    }
  }

  year <- strftime(startdate, "%Y", tz = "UTC")

  # ----- Style ----------------------------------------------------------------

  if (style == "large") {
    pointsize <- 2
    linesize <- 0.8
    date_labels <- "%b %d"
    minor_break_width <- NULL
    base_size <- 15
  } else if (style == "small") {
    pointsize <- 2
    linesize <- 0.8
    date_labels <- "%b\n%d"
    minor_break_width <- "6 hours"
    base_size <- 11
  }

  # ----- Create plot ----------------------------------------------------------

  plot <-
    ggplot_pm25Timeseries(
      mts_tidy,
      startdate = startdate,
      enddate = enddate,
      includeFullEnddate = FALSE,
      date_labels = date_labels,
      minor_break_width = minor_break_width,
      base_size = base_size,
      xlab = year,
      ...
    ) +
    custom_aqiLines(size = 1, alpha = .8) +
    geom_pm25Points(mapping_pm25, size = pointsize) +
    stat_nowcast(mapping_nowcast, size = linesize) +
    custom_aqiStackedBar() +
    ggtitle(title)


  # ----- Handle legend --------------------------------------------------------

  # Add legend when plotting multiple monitors
  if (length(unique(mts_tidy$deviceDeploymentID)) == 1) {

    values <- c(1, 1)
    names(values) <- c(pm25LegendLabel, nowcastLegendLabel)
    scale <- scale_color_manual(name = "", values = values, labels = names(values))
    guide <- guides(
      color = guide_legend(
        title = "",
        override.aes = list(
          color = c(1, 1),
          size = c(pointsize, linesize),
          linetype = c(NA, 1),
          shape = c(16, NA),
          alpha = c(0.3, 1)
        )
      )
    )

    plot <- plot + scale + guide

  # Single monitor
  } else {
    plot <- plot + scale_color_brewer(palette = "Dark2")
  }

  plot <- plot + theme_timeseriesPlot_airfire(size = style)

  # ----- Return ---------------------------------------------------------------

  return(plot)

}
