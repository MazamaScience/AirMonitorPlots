#' @export
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @title Create a daily barplot for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' daily barplot for one or more monitors.
#'
#' The full range of data in \code{monitor} will be used unless both
#' \code{startdate} and \code{enddate} are specified.
#'
#' @param monitor A \emph{mts_monitor} object.
#' @param startdate Desired start date (integer or character in ymd format or
#'   POSIXct).
#' @param enddate Desired end date (integer or character in ymd format or
#'   POSIXct).
#' @param id deviceDeploymentID to include in the plot. This can be NULL if
#'   \code{monitor} only has one unique deviceDeploymentID.
#' @param style String indicating plotting style. Either \code{"large"} or
#'   \code{"small"}. \code{style = "large"} is suitable for plots larger than
#'   450x450px, and \code{"small"} is suitable for plots 450x450px or smaller.
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param timezone Olson timezone name for x-axis scale and date parsing. If
#'   NULL the timezone of the specified monitor will be used.
#' @param today Logical indicating whether to include a shaded "current NowCast"
#'   bar for Today. Ignored if data is not current.
#' @param ... Arguments passed onto \code{\link{ggplot_pm25Timeseries}}.
#'
#' @return A \emph{ggplot} object
#'
#' @examples
#' library(AirMonitorPlots)
#'
#' AirMonitor::Carmel_Valley %>%
#'   monitor_ggDailyBarplot(
#'     startdate = 20160801,
#'     enddate = 20160810
#'   )
#'

monitor_ggDailyBarplot <- function(
  monitor,
  startdate = NULL,
  enddate = NULL,
  id = NULL,
  style = "small",
  title = NULL,
  timezone = NULL,
  today = TRUE,
  ...
) {

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  # Check style
  if ( !style %in% c("small", "large") )
    stop("Invalid style. Choose from 'small' or 'large'.")

  # Check today bar inclusion
  if ( !is.logical(today) )
    stop("'today' must be a logical (TRUE or FALSE).")

  # Convert monitor to tidy structure
  mts_tidy <- monitor_toTidy(monitor)

  # Check deviceDeploymentID
  if ( is.null(id) ) {

    if (length(unique(mts_tidy$deviceDeploymentID)) > 1) {
      stop("id is required if monitor has multiple monitors.")
    } else {
      id <- mts_tidy$deviceDeploymentID[1]
    }

  } else {

    if ( length(id) > 1 ) {
      stop("'id' must contain a single deviceDeploymentID.")
    } else if (!id %in% unique(mts_tidy$deviceDeploymentID)) {
      stop("deviceDeploymentID not present in data.")
    }
  }

  # NOTE: Include before getting timezone
  mts_tidy <- dplyr::filter(mts_tidy, .data$deviceDeploymentID == !!id)

  # Check timezone
  if ( !is.null(timezone) ) {
    if ( !timezone %in% OlsonNames() ) {
      stop("Invalid timezone")
    }
  } else {
    timezone <- unique(mts_tidy$timezone)
  }


  # ----- Prepare data ---------------------------------------------------------

  # Use full time range if startdate or enddate is missing
  if ( is.null(startdate) || is.null(enddate) ) {
    timeRange <- range(mts_tidy$datetime)
    startdate <- timeRange[1]
    enddate <- timeRange[2]
  }

  dateRange <- MazamaCoreUtils::dateRange(
    startdate = startdate,
    enddate = enddate,
    timezone = timezone,
    unit = "day",
    ceilingEnd = TRUE
  )

  startdate <- dateRange[1]
  enddate <- min(c(dateRange[2], lubridate::now(tzone = timezone)))

  mts_tidy <- mts_tidy %>%
    dplyr::filter(
      .data$datetime >= startdate,
      .data$datetime < enddate
    )


  # ----- Style ----------------------------------------------------------------

  # Get title
  if ( is.null(title) ) {
    title <- paste0("Daily Average PM2.5", "\n", "Site: ", unique(mts_tidy$locationName))
  }

  if ( style == "large" ) {
    nowcastTextSize <- 4.5
    nowcastText <- "Current\nNowCast"
    date_format <- "%b %d"
    base_size <- 15
  } else if ( style == "small" ) {
    nowcastTextSize <- 4
    nowcastText <- "Now-\nCast"
    date_format <- "%b\n%d"
    base_size <- 11
  }

  # Check for any data from "today"
  if ( isFALSE(enddate > lubridate::floor_date(lubridate::now(tzone = timezone), unit = "day")) ) {
    today <- FALSE
  }

  # Create "current nowcast" bar
  if ( today ) {

    lastValidIndex <- dplyr::last(which(!is.na(mts_tidy$pm25)))
    lastValidDatetime <- mts_tidy$datetime[lastValidIndex]

    todayHour <-
      lubridate::with_tz(lastValidDatetime, tzone = timezone) %>%
      lubridate::hour()

    now <- lubridate::now(tzone = timezone)

    # Don't show 'current nowcast' before 5am
    if ( todayHour < 5 ) {
      ## TODO: Handle missing 'current nowcast'
      currentNowcast <- 0
    } else {
      nowcast <- .nowcast(mts_tidy$pm25)
      currentNowcast <- nowcast[lastValidIndex]
    }

    center <- lubridate::floor_date(now, "day") + lubridate::dhours(12)
    left <- center - (0.8 / 2 * 86400)
    right <- center + (0.8 / 2 * 86400)

    color <- AirMonitor::aqiColors(
      x = currentNowcast,
      pollutant = "AQI",
      palette = "EPA",
      na.color = "gray60"
    )

    rect <- annotate(
      "rect",
      xmin = left,
      xmax = right,
      ymin = 0,
      ymax = currentNowcast,
      fill = color,
      color = "gray60",
      alpha = .8
    )

    rect2 <- annotate(
      "rect",
      xmin = left,
      xmax = right,
      ymin = 0,
      ymax = currentNowcast,
      fill = "gray60",
      color = "gray60",
      alpha = .3
    )

    text <- annotate(
      "text",
      y = 0,
      x = center,
      label = nowcastText,
      vjust = 1.5,
      color = "gray40",
      size =  nowcastTextSize
    )

    nowcastBar <- list(
      rect,
      rect2,
      text,
      coord_cartesian(clip = "off") # Turn off clipping so labels can be added outside of plot region
    )

  } else {

    nowcastBar <- list()

  }

  # ----- Create plot ----------------------------------------------------------

  plot <-
    ggplot_pm25Timeseries(
      mts_tidy,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      date_labels = date_format,
      tick_location = "midday",
      today_label = !today,
      base_size = base_size,
      ...
    ) +
    custom_aqiLines(size = 1, alpha = .8) +
    stat_dailyAQCategory(timezone = timezone, adjustylim = TRUE, color = "black") +
    custom_aqiStackedBar(width = .015) +

    ## Format/theme tweaks
    # Remove padding on y scale
    scale_y_continuous(expand = c(0, 0)) +
    ggtitle(title) +
    xlab(xlab) +
    nowcastBar +
    theme_dailyBarplot_airfire(size = style)

  return(plot)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  monitor <- airnow_loadLatest()

  startdate = NULL
  enddate = NULL
  id = "a9572a904a4ed46d_060530002" # Carmel Valley
  style = "small"
  title = NULL
  timezone = NULL
  today = TRUE


  monitor_ggDailyBarplot(
    monitor = monitor,
    startdate = startdate,
    enddate = enddate,
    id = id,
    style = style,
    title = title,
    timezone = timezone,
    today = today
  )


}
