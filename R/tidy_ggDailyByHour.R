#' @title Create a daily barplot timeseries for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' dailyByHour diurnal plot for one monitor.  
#'
#' @inheritParams ggplot_pm25Diurnal
#' @param ws_tidy dataframe of monitor data, created from a \code{ws_monitor}
#' object using \code{monitor_toTidy()}. 
#' @param monitorID monitorID to include in the plot. 
#' @param style String indicating plotting style. Either \code{"large"} or \code{"small"}.
#' \code{style = "large"} is suitable for plots larger than 450x450px, and \code{"small"}
#' is suitable for plots 450x450px or smaller. 
#' @param title Plot title. If NULL, a suitable title will be constructed.
#' @param timezone Timezone for x-axis scale. If NULL and only one timezone present
#' in the data, the data timezone will be used. If NULL and multiple timezones 
#' present, the default is UTC. 
#' @return A **ggplot** object
#'
#' @import ggplot2
#' @importFrom rlang .data
#' 
#' @export
#' 
#' @examples
#' ws_monitor <- airnow_loadLatest()
#' ws_tidy <- monitor_toTidy(ws_monitor)
#' tidy_ggDailyByHour(ws_tidy, monitorID = "060631010_01")


tidy_ggDailyByHour <- function(ws_tidy,
                               startdate = NULL,
                               enddate = NULL,
                               monitorID = NULL,
                               style = "large",
                               title = NULL,
                               timezone = NULL,
                               ...) {
  

  
  # Sanity checks
  if (!monitor_isTidy(ws_tidy)) {
    stop("ws_tidy must be ws_tidy objec")
  }
  
  if (any(!monitorID %in% unique(ws_tidy$monitorID))) {
    invalidIDs <- monitorID[which(!monitorID %in% unique(ws_tidy$monitorID))]
    stop(paste0("monitorID not present in data: ", paste0(invalidIDs, collapse = ", ")))
  }
  
  if ( !is.null(startdate) & !is.null(enddate) ) {
    daterange <- range(ws_tidy$datetime)
    if ( parseDatetime(startdate) > daterange[2] ) {
      stop("startdate is outside of data date range")
    } 
    if ( parseDatetime(enddate) < daterange[1] ) {
      stop("enddate is outside of data date range")
    }
  }
  
  # Subset Data
  if (!is.null(monitorID)) {
    ws_tidy <- dplyr::filter(.data = ws_tidy, .data$monitorID == !!monitorID)
  }
  
  
  if (!is.null(timezone)) {
    if (!timezone %in% OlsonNames()) {
      stop("Invalid Timezone")
    }
  } else {
    timezone <- unique(ws_tidy$timezone)
  }
  
  if (length(unique(ws_tidy$monitorID)) > 1 & is.null(monitorID)) {
    stop("Specify monitorID")
  }
  
  # Subset based on startdate and enddate
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
  
  
  # Get title
  if (is.null(title)) {
    title <- paste0("NowCast by Time of Day\n",
                    "Site: ", unique(ws_tidy$siteName)) 
  }
  
  
  # Add column for 'hour', 'day', and 'nowcast'. 
  ws_tidy$hour <- as.numeric(strftime(ws_tidy$datetime, "%H", tz = timezone))
  ws_tidy$day  <- strftime(ws_tidy$datetime, "%Y%m%d", tz = timezone)
  ws_tidy$nowcast <- .nowcast(ws_tidy$pm25)
  
  # Get 'yesterday' and 'today'. 
  today_string <- strftime(enddate, "%Y%m%d")
  yesterday_string <- strftime(enddate - lubridate::ddays(1), "%Y%m%d")
  yesterday <- dplyr::filter(ws_tidy, .data$day == yesterday_string)
  today <- dplyr::filter(ws_tidy, .data$day == today_string)
  
  # Get labels for legend
  meanText <- paste0(as.integer(difftime(enddate, startdate, units = "days")), " Day Mean")
  
  # Set custom styling 
  # Custom style formatting
  if (style == "large") {
    meanSize <- 8
    yesterdayPointSize <- 4
    yesterdayLineSize <- .9
    todayPointSize <- 5
    todayLineSize <- 1.3
    custom_theme <- theme(plot.margin = margin(
                            unit(25, "pt"),    # Top
                            unit(10, "pt"),    # Right
                            unit(25, "pt"),    # Bottom
                            unit(10, "pt")     # Left
                          ),
                          axis.text = element_text(size = 12),
                          axis.title = element_text(size = 18),
                          plot.title = element_text(size = 20))
  } else if (style == "small") {
    meanSize <- 5
    yesterdayPointSize <- 2.8
    yesterdayLineSize <- .5
    todayPointSize <- 3
    todayLineSize <- 1
    custom_theme <- theme(axis.title.x.bottom = element_blank(),
                          plot.margin = margin(
                            unit(20, "pt"),    # Top
                            unit(10, "pt"),    # Right
                            unit(15, "pt"),    # Bottom
                            unit(10, "pt")     # Left
                          ),
                          axis.text = element_text(size = 12),
                          axis.title.y = element_text(size = 12),
                          plot.title = element_text(size = 15))
  }
  
  # Make the plot
  plot <- ggplot_pm25Diurnal(ws_tidy, 
                     startdate = startdate,
                     enddate = enddate,
                     mapping = aes_(x = ~hour, y = ~nowcast),
                     ...) +
    custom_aqiLines() +
    custom_aqiStackedBar() +
    stat_meanByHour(aes(color = !!meanText), geom = "line", size = meanSize, alpha = .3, lineend = "round") +
    geom_line(aes(color = "Yesterday"), data=yesterday, size = yesterdayLineSize) +
    stat_AQILevel(aes(color = "Yesterday"), data = yesterday, geom = "point", nowcast = FALSE, shape = 21, size = yesterdayPointSize) +
    geom_line(aes(color = "Today"), data=today, size = todayLineSize) +
    stat_AQILevel(aes(color = "Today"), data = today, geom = "point", nowcast = FALSE, shape = 21, size = todayPointSize)  +
    ggtitle(title) +
    custom_theme
  
  # Add legend
  values <- c("black", "gray50", "black")
  names(values) <- c("Today", "Yesterday", meanText)
  scale <- scale_color_manual(name = "", values = values, labels = names(values))
  guide <- guides(color = guide_legend(title = "", 
                                       override.aes = list(
                                         fill = c("green", "green", NA),
                                         color = c("black", "gray50", "black"),
                                         shape = c(21, 21, NA),
                                         cex = c(todayPointSize, yesterdayPointSize, meanSize),
                                         linetype = c(NA, NA, 1),
                                         lineend = c(NA, NA, "round"),
                                         alpha = c(1, 1, .1)
                                       )))
  plot + scale + guide +
    theme(legend.key.size = unit(1, "cm"),
          legend.position = "top",
          legend.text = element_text(size = 12,
                                     face = "italic",
                                     margin = margin(r = 50)),
          legend.text.align = 1)

  
}
