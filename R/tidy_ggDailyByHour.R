#' @title Create a daily barplot timeseries for one or more monitors
#'
#' @description
#' This function assembles various layers to create a production-ready
#' dailyByHour diurnal plot for one monitor.  
#'
#' @param ws_tidy dataframe of monitor data, created from a \code{ws_monitor}
#' object using \code{monitor_toTidy()}. 
#' @param startdate Desired start date (integer or character in ymd format or POSIXct)
#' @param enddate Desired end date (integer or character in ymd format or POSIXct)
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
                               style = NULL,
                               title = NULL,
                               timezone = NULL) {
  

  
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
  
  if (!is.null(timezone)) {
    if (!timezone %in% OlsonNames()) {
      stop("Invalid Timezone")
    }
  } else {
    
  }
  
  if (length(unique(ws_tidy$monitorID)) > 1 & is.null(monitorID)) {
    stop("Specify monitorID")
  }
  
  # Subset based on startdate and enddate
  if (!is.null(startdate)) {
    s <- parseDatetime(startdate, timezone = timezone)
    ws_tidy <- dplyr::filter(ws_tidy, .data$datetime >= lubridate::floor_date(s, unit = "day"))
  }
  if (!is.null(enddate)) {
    e <- parseDatetime(enddate, timezone = timezone)
    ws_tidy <- dplyr::filter(ws_tidy, .data$datetime <= lubridate::ceiling_date(e, unit = "day"))
  }
  
  # Subset Data
  if (!is.null(monitorID)) {
    ws_tidy <- dplyr::filter(.data = ws_tidy, .data$monitorID == !!monitorID)
  }
  timezone <- unique(ws_tidy$timezone)
  
  # Add column for 'hour', 'day', and 'nowcast'. 
  ws_tidy$hour <- as.numeric(strftime(ws_tidy$datetime, "%H", tz = timezone))
  ws_tidy$day  <- strftime(ws_tidy$datetime, "%Y%m%d", tz = timezone)
  ws_tidy$nowcast <- .nowcast(ws_tidy$pm25)
  
  # Get 'yesterday' and 'today'. 
  today_string <- strftime(lubridate::today(timezone), "%Y%m%d")
  yesterday_string <- strftime(lubridate::today(timezone) - lubridate::ddays(1), "%Y%m%d")
  yesterday <- filter(ws_tidy, day == yesterday_string)
  today <- filter(ws_tidy, day == today_string)
  
  ggplot_pm25Diurnal(ws_tidy, 
                     startdate = startdate,
                     enddate = enddate) +
    stat_meanByX(geom = "line", size = 5, alpha = .3, lineend = "round") +
    geom_line(aes(y = nowcast), data=yesterday, color = "gray50") +
    stat_AQILevel(aes(y = nowcast), data = yesterday, geom = "point", nowcast = FALSE, shape = 21, color = "gray50", size = 3) +
    geom_line(aes(y = nowcast), data=today, size = 1) +
    stat_AQILevel(aes(y = nowcast), data = today, geom = "point", nowcast = FALSE, shape = 21, color = "black", size = 4) 
    # custom_legend(labels = c("Today", "Yesterday", paste0(length(unique(ws_tidy$day)), "-day Mean")),
    #               aesthetics = list(size = c(5, 1, .8),
    #                                 linetype = c(1, 1, 1),
    #                                 shape = c(23, 23, NA),
    #                                 alpha = c(NA, NA, .3)))
  
  
 }

if (FALSE) {
  # for testing
  library(PWFSLSmokePlots)
  monitorID <- "060631010_01"
  ws_monitor <- airnow_loadLatest()
  startdate <- lubridate::floor_date(lubridate::now() - lubridate::ddays(6), "day")
  enddate <- lubridate::now()
}
