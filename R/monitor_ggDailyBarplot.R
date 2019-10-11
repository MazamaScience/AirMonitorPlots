#' @title Create a daily barplot for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' daily barplot for one or more monitors.
#'
#' The full range of data in \code{ws_monitor} will be used unless both
#' \code{startdate} and \code{enddate} are specified.
#'
#' @param ws_monitor A \code{ws_monitor} object.
#' @param startdate Desired start date (integer or character in ymd format or
#'   POSIXct).
#' @param enddate Desired end date (integer or character in ymd format or
#'   POSIXct).
#' @param monitorID monitorID to include in the plot. This can be NULL if
#'   \emph{ws_monitor} only has one unique monitorID.
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
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' monitor_ggDailyBarplot(ws_monitor, startdate = 20160801, enddate = 20160810)
#'
#' \dontrun{
#' ws_monitor <- airnow_loadLatest()
#' monitor_ggDailyBarplot(ws_monitor, monitorIDs = "410432002_01", today = TRUE)
#' }
monitor_ggDailyBarplot <- function(
  ws_monitor,
  startdate = NULL,
  enddate = NULL,
  monitorID = NULL,
  style = "small",
  title = NULL,
  timezone = NULL,
  today = TRUE,
  ...
) {

  # Validate Parameters --------------------------------------------------------

  # Convert ws_monitor to tidy structure
  if ( monitor_isMonitor(ws_monitor) ) {
    ws_tidy <- monitor_toTidy(ws_monitor)
  } else {
    stop("ws_monitor is not a ws_monitor object.")
  }

  # Check style
  if ( !style %in% c("small", "large") )
    stop("Invalid style. Choose from 'small' or 'large'.")

  # Check today bar inclusion
  if ( !is.logical(today) )
    stop("'today' must be a logical (TRUE or FALSE).")

  # Check monitorID
  if ( is.null(monitorID) ) {

    if (length(unique(ws_tidy$monitorID)) > 1) {
      stop("monitorID is required if `ws_monitor` has multiple monitors.")
    } else {
      monitorID <- ws_tidy$monitorID[1]
    }

  } else {

    if (length(monitorID) > 1) {
      stop("`monitorID` must contain a single monitorID.")
    } else if (!monitorID %in% unique(ws_tidy$monitorID)) {
      stop("monitorID not present in data.")
    }
  }

  # NOTE: Include before getting timezone
  ws_tidy <- dplyr::filter(ws_tidy, .data$monitorID == !!monitorID)

  # Check timezone
  if ( !is.null(timezone) ) {
    if (!timezone %in% OlsonNames()) {
      stop("Invalid timezone")
    }
  } else {
    timezone <- unique(ws_tidy$timezone)
  }


  # Prepare data ---------------------------------------------------------------

  # Use full time range if startdate or enddate is missing
  if ( is.null(startdate) || is.null(enddate) ) {
    timeRange <- range(ws_tidy$datetime)
    startdate <- timeRange[1]
    enddate <- timeRange[2]
  }

  dateRange <- MazamaCoreUtils::dateRange(
    startdate = startdate,
    enddate = enddate,
    timezone = timezone,
    unit = "day"
  )

  startdate <- dateRange[1]
  enddate <- min(c(dateRange[2], lubridate::now(tzone = timezone)))

  ws_tidy <- ws_tidy %>%
    dplyr::filter(
      .data$datetime >= startdate,
      .data$datetime < enddate
    )


  # Style ----------------------------------------------------------------------

  # Get title
  if (is.null(title)) {
    title <- paste0("Daily Average PM2.5", "\n", "Site: ", unique(ws_tidy$siteName))
  }

  if (style == "large") {
    nowcastTextSize <- 4.5
    nowcastText <- "Current\nNowCast"
    date_format <- "%b %d"
    base_size <- 15
  } else if (style == "small") {
    nowcastTextSize <- 4
    nowcastText <- "Now-\nCast"
    date_format <- "%b\n%d"
    base_size <- 11
  }

  # Set "today"
  if (isFALSE(all.equal(enddate, lubridate::ceiling_date(lubridate::now(tzone = timezone), "day")))) {
    today <- FALSE
  }

  # Create "current nowcast" bar
  if (today) {

    now <- lubridate::now(tzone = timezone)
    lastValidIndex <- dplyr::last(which(!is.na(ws_tidy$pm25)))

    if (now - ws_tidy$datetime[lastValidIndex] > lubridate::dhours(5)) {
      ## TODO: Handle missing 'current nowcast'
      currentNowcast <- 0
    } else {
      nowcast <- .nowcast(ws_tidy$pm25)
      currentNowcast <- nowcast[lastValidIndex]
    }

    center <- lubridate::floor_date(now, "day") + lubridate::dhours(12)
    left <- center - (0.8 / 2 * 86400)
    right <- center + (0.8 / 2 * 86400)

    color <- AQI$colors[.bincode(currentNowcast, AQI$breaks_24)]

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

  # Create plot ----------------------------------------------------------------

  plot <-
    ggplot_pm25Timeseries(
      ws_tidy,
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
    theme_dailyBarplot_pwfsl(size = style)

  return(plot)

}
