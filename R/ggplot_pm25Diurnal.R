#' @title Instantiate a pm25 timeseries ggplot
#'
#' @description
#' Create a plot using ggplot with default mappings and styling. Layers can then 
#' be added to this plot using \code{ggplot2} syntax. 
#'
#' @inheritParams custom_pm25DiurnalScales
#' @param ws_data Default dataset to use when adding layers. Must be either a \code{ws_monitor} 
#' object or \code{ws_tidy} object. 
#' @param startdate Desired startdate for data to include, in a format that can be 
#' parsed with \link{parseDatetime}.
#' @param enddate Desired enddate for data to include, in a format that can be parsed 
#' with \link{parseDatetime}.
#' @param timezone Timezone to use to set hours of the day
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' ggplot_pm25Diurnal(ws_monitor) +
#'   coord_polar() +
#'   geom_pm25Points() +
#'   custom_aqiStackedBar(width = 1, alpha = .3) 
#' 
#' ggplot_pm25Diurnal(ws_monitor, 
#'                    startdate = 20160801, 
#'                    enddate = 20160810) + 
#'   stat_boxplot(aes(group = hour)) 



ggplot_pm25Diurnal <- function(ws_data,
                                  startdate = NULL,
                                  enddate = NULL,
                                  timezone = NULL,
                                  ylim = NULL) {
  
  # Sanity checks
  if ( monitor_isMonitor(ws_data) ) {
    ws_tidy <- monitor_toTidy(ws_data)
  } else if ( monitor_isTidy(ws_data) ) {
    ws_tidy <- ws_data
  } else {
    stop("ws_data must be either a ws_monitor object or ws_tidy object.")
  }
  
  if (!is.null(startdate) & !is.null(enddate)) {
    daterange <- range(ws_tidy$datetime)
    if ( !lubridate::`%within%`(parseDatetime(startdate), lubridate::interval(daterange[1], daterange[2])) ) {
      stop("startdate is outside of data date range")
    } 
    if ( !lubridate::`%within%`(parseDatetime(enddate), lubridate::interval(daterange[1], daterange[2])) ) {
      stop("enddate is outside of data date range")
    }
  }
  
  # Get timezone
  if ( is.null(timezone) ) {
    if (length(unique(ws_tidy$timezone)) > 1) {
      timezone <- "UTC"
      xlab <- "Time of Day (UTC)"
    } else {
      timezone <- ws_tidy$timezone[1]
      xlab <- "Time of Day (Local)"
    }
  } else if ( is.null(xlab) ) {
    xlab <- paste0("Time of Day (", timezone, ")")
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
  
  
  # Add column for 'hour'
  ws_tidy$hour <- as.numeric(strftime(ws_tidy$datetime, "%H", tz = timezone))
  ws_tidy$day  <- strftime(ws_tidy$datetime, "%Y%m%d", tz = timezone)
  
  ggplot(ws_tidy, aes_(x = ~hour, y = ~pm25)) +
    theme_timeseriesPlot_pwfsl() +
    custom_pm25DiurnalScales(ws_tidy,
                             xlab = xlab)
  
}

