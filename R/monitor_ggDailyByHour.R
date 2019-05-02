#' @title Create a diurnal plot for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' diurnal plot for one or more monitors.
#'
#' @inheritParams ggplot_pm25Diurnal
#' @param ws_monitor A \code{ws_monitor} object.
#' @param monitorID monitorID to include in the plot.
#' @param style String indicating plotting style. Either \code{"large"} or
#'   \code{"small"}. \code{style = "large"} is suitable for plots larger than
#'   450x450px, and \code{"small"} is suitable for plots 450x450px or smaller.
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param timezone Timezone for x-axis scale. If NULL and only one timezone
#'   present in the data, the data timezone will be used. If NULL and multiple
#'   timezones present, the default is UTC.
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

  if (monitor_isMonitor(ws_monitor)) {
    ws_tidy <- monitor_toTidy(ws_monitor)
  } else {
    stop("ws_monitor is not a ws_monitor object.")
  }

  if (!style %in% c("small", "large"))
    stop("Invalid style. Choose from 'small' or 'large'.")

  if (any(!monitorID %in% unique(ws_tidy$monitorID))) {
    invalidIDs <- monitorID[which(!monitorID %in% unique(ws_tidy$monitorID))]
    stop(paste0(
      "Invalid monitorID. monitorID not present in data: ",
      paste0(invalidIDs, collapse = ", ")
    ))
  }

  if (!is.null(startdate) && parseDatetime(startdate) > range(ws_tidy$datetime)[2]) {
    stop("startdate is outside of data date range")
  }
  if (!is.null(enddate) && parseDatetime(enddate) < range(ws_tidy$datetime)[1]) {
    stop("enddate is outside of data date range")
  }

  # Prepare data ---------------------------------------------------------------

  if (!is.null(monitorID)) {
    ws_tidy <- dplyr::filter(ws_tidy, .data$monitorID == !!monitorID)
  }

  if (length(unique(ws_tidy$monitorID)) > 1 & is.null(monitorID)) {
    stop("monitorID must be specified")
  }

  if (!is.null(timezone)) {
    if (!timezone %in% OlsonNames()) {
      stop("Invalid timezone")
    }
  } else {
    timezone <- unique(ws_tidy$timezone)
  }


  # * Subset time range --------------------------------------------------------

  if (!is.null(startdate)) {
    startdate <- parseDatetime(startdate, timezone = timezone)
    ws_tidy <- dplyr::filter(ws_tidy, .data$datetime >= startdate)
  } else {
    startdate <- min(ws_tidy$datetime)
  }

  if (!is.null(enddate)) {
    enddate <- parseDatetime(enddate, timezone = timezone)
    if (enddate == lubridate::floor_date(enddate, "day")) {
      enddate <- lubridate::ceiling_date(enddate, "day") - lubridate::dminutes(1)
    }
    ws_tidy <- dplyr::filter(ws_tidy, .data$datetime <= enddate)
  } else {
    enddate <- max(ws_tidy$datetime)
  }


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

  today_string <- strftime(enddate, "%Y%m%d")
  yesterday_string <- strftime(enddate - lubridate::ddays(1), "%Y%m%d")
  yesterday <- dplyr::filter(ws_tidy, .data$day == yesterday_string)
  today <- dplyr::filter(ws_tidy, .data$day == today_string)


  # Style ----------------------------------------------------------------------

  # Get title
  if (is.null(title)) {
    title <- paste0("NowCast by Time of Day\n",
                    "Site: ", unique(ws_tidy$siteName))
  }

  # Get labels for legend
  meanText <- paste0(as.integer(difftime(enddate, startdate, units = "days")), " Day Mean")

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

  plot <-
    ggplot_pm25Diurnal(
      ws_tidy,
      startdate = startdate,
      enddate = enddate,
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
    ) +
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
    ) +
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
    )  +
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

  plot <- plot + scale + guide

  return(plot)

}
