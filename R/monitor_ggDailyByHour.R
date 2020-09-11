#' @title Create a diurnal plot for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' diurnal plot for a single monitor.
#'
#' The full range of data in \code{ws_monitor} will be used unless both
#' \code{startdate} and \code{enddate} are specified.
#'
#' @inheritParams ggplot_pm25Diurnal
#' @param ws_monitor A \code{ws_monitor} object.
#' @param monitorID monitorID to include in the plot. This can be NULL if
#'   \emph{ws_monitor} only has one unique monitorID.
#' @param style String indicating plotting style. Either \code{"large"} or
#'   \code{"small"}. \code{style = "large"} is suitable for plots larger than
#'   450x450px, and \code{"small"} is suitable for plots 450x450px or smaller.
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param timezone Olson timezone name for x-axis scale and date parsing. If
#'   NULL the timezone of the specified monitor will be used.
#'
#' @return A \emph{ggplot} object.
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ws_monitor <- airnow_loadLatest()
#' monitor_ggDailyByHour(ws_monitor, monitorID = "410432002_01")
#' }
#'
#' ws_monitor <- Carmel_Valley
#' monitor_ggDailyByHour(ws_monitor, startdate = 20160801, enddate = 20160810)
monitor_ggDailyByHour <- function(
  ws_monitor,
  startdate = NULL,
  enddate = NULL,
  monitorID = NULL,
  style = "small",
  title = NULL,
  timezone = NULL,
  ...
) {


  # Validate Parameters --------------------------------------------------------

  # Convert ws_monitor to tidy structure
  if (monitor_isMonitor(ws_monitor)) {
    ws_tidy <- monitor_toTidy(ws_monitor)
  } else {
    stop("ws_monitor is not a ws_monitor object.")
  }

  # Check style
  if (!style %in% c("small", "large"))
    stop("Invalid style. Choose from 'small' or 'large'.")

  # Check monitorID
  if (is.null(monitorID)) {

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
    if ( !timezone %in% OlsonNames() ) {
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
    unit = "day",
    ceilingEnd = TRUE
  )

  ws_tidy <-
    ws_tidy %>%
    dplyr::filter(
      .data$datetime >= dateRange[1],
      .data$datetime < dateRange[2]
    )


  # * Add 'hour', 'day', and 'nowcast' columns ---------------------------------

  ## NOTE:
  #  Prefixing `timezone` with `!!` tells dplyr to use the variable `timezone`
  #  instead of the column "timezone" in `ws_tidy`.

  ws_tidy <- ws_tidy %>%
    dplyr::mutate(
      hour = as.numeric(strftime(.data$datetime, "%H", tz = !!timezone)),
      day = strftime(.data$datetime, "%Y%m%d", tz = !!timezone),
      nowcast = .nowcast(.data$pm25)
    )


  # * Separate data for 'yesterday' and 'today' --------------------------------

  today_string <- dateRange[2] %>%
    magrittr::subtract(lubridate::days(1)) %>%
    strftime("%Y%m%d", tz = timezone)
  yesterday_string <- dateRange[2] %>%
    magrittr::subtract(lubridate::days(2)) %>%
    strftime("%Y%m%d", tz = timezone)

  yesterday <- dplyr::filter(ws_tidy, .data$day == yesterday_string)
  today <- dplyr::filter(ws_tidy, .data$day == today_string)


  # Style ----------------------------------------------------------------------

  # Get title
  if (is.null(title)) {
    title <- paste0("NowCast by Time of Day\n",
                    "Site: ", unique(ws_tidy$siteName))
  }

  # Get labels for legend
  meanText <- paste0(as.integer(difftime(dateRange[2], dateRange[1], units = "days")), " Day Mean")

  if (style == "large") {
    meanSize <- 8
    yesterdayPointSize <- 4
    yesterdayLineSize <- .9
    todayPointSize <- 5
    todayLineSize <- 1.3
    base_size <- 15
  } else if (style == "small") {
    meanSize <- 5
    yesterdayPointSize <- 2.8
    yesterdayLineSize <- .5
    todayPointSize <- 3
    todayLineSize <- 1
    base_size <- 11
  }


  # Create plot ----------------------------------------------------------------

  gg <-
    ggplot_pm25Diurnal(
      ws_tidy,
      startdate = dateRange[1],
      enddate = dateRange[2],
      mapping = aes_(x = ~ hour, y = ~ nowcast),
      base_size = base_size,
      ...
    ) +
    custom_aqiLines() +
    custom_aqiStackedBar() +
    # large mean line
    stat_meanByHour(
      aes(color = !!meanText),
      geom = "line",
      size = meanSize,
      alpha = .3,
      lineend = "round"
    )

  if ( nrow(yesterday) > 0 ) {
    gg <- gg +
      # Yesterday line
      geom_line(aes(color = "Yesterday"), data = yesterday, size = yesterdayLineSize) +
      # Yesterday points
      stat_AQCategory(
        aes(color = "Yesterday"),
        data = yesterday,
        geom = "point",
        nowcast = FALSE,
        shape = 21,
        size = yesterdayPointSize
      )
  }

  if ( nrow(today) > 0 ) {
    gg <- gg +
      # Today line
      geom_line(aes(color = "Today"), data = today, size = todayLineSize) +
      # Today points
      stat_AQCategory(
        aes(color = "Today"),
        data = today,
        geom = "point",
        nowcast = FALSE,
        shape = 21,
        size = todayPointSize
      )
  }

  gg <- gg +
    # Title
    ggtitle(title) +
    # Theme
    theme_dailyByHour_pwfsl(size = style)


  # * Add legend ---------------------------------------------------------------

  values <- c("black", "gray50", "black")
  names(values) <- c("Today", "Yesterday", meanText)
  scale <- scale_color_manual(name = "", values = values, labels = names(values))
  guide <- guides(
    color = guide_legend(
      title = "",
      override.aes = list(
        fill = c("green", "green", NA),
        color = c("black", "gray50", "black"),
        shape = c(21, 21, NA),
        cex = c(todayPointSize, yesterdayPointSize, meanSize),
        linetype = c(NA, NA, 1),
        lineend = c(NA, NA, "round"),
        alpha = c(1, 1, .1)
      )
    )
  )

  gg <- gg + scale + guide

  return(gg)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  # BUG ==> ggplot errors when plotting ddata right after UTC midnight
  # Most likely issue is today/yesterday tibbles with zero rows. We should
  # check for this before these separate, custom points

  dateRange <- MazamaCoreUtils::dateRange(enddate = 20190828,
                                          timezone = "UTC",
                                          unit = "hour")

  startdate <- dateRange[1]
  enddate <- dateRange[2]

  monitorID <- "530030004_01"

  ws_monitor <-
    monitor_load(startdate, enddate, monitorID)

  style <- "small"
  title <- NULL
  timezone <- "UTC"

}
