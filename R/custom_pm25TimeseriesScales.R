#' @title PWFSL PM2.5 timeseries scales
#'
#' @description
#' Add PWFSL-style x-axis and y-axis scales suitable for a timeseries plot 
#' showing PM2.5 data. 
#'
#' @param data pm25 timeseries data. Should match the default dataset of the
#' plot
#' @param startdate Desired startdate for x-axis, in a format that can be 
#' parsed with \link{parseDatetime}.
#' @param enddate Desired enddate for x-axis, in a format that can be parsed 
#' with \link{parseDatetime}.
#' @param ylim custom y-axis limits. This function will apply a default limit
#' depending on the data. 
#' @param timezone Timezone for x-axis scale. If NULL and only one timezone present
#' in the data, the data timezone will be used. If NULL and multiple timezones 
#' present, the default is UTC. 
#' 
#' @importFrom rlang .data
#' @import ggplot2
#' @export
#' 

custom_pm25TimeseriesScales <- function(data = NULL,
                                        startdate = NULL, 
                                        enddate = NULL, 
                                        ylim = NULL,
                                        timezone = NULL) {
  
  if (is.null(data)) {
    if (is.null(startdate) || is.null(enddate) || is.null(ylim)) {
      stop("at least one of data, startdate, enddate, and ylim must be specified.")
    }
  }
  
  if ( monitor_isMonitor(data) ) {
    data <- monitor_toTidy(data)
  } else if ( monitor_isTidy(data) ) {
    data <- data
  } else {
    stop("data must be either a ws_monitor object or ws_tidy object.")
  }
  
  if (is.null(startdate)) {
    startdate <- min(data$datetime)
  }
  if (is.null(enddate)) {
    enddate <- max(data$datetime)
  }
  
  if (is.null(timezone)) {
    if (length(unique(data$timezone)) > 1) {
      timezone <- "UTC"
      xlab <- "Time (UTC)"
    } else {
      timezone <- data$timezone[1]
      xlab <- "Local Time"
    }
  }
  
  # handle various startdates
  if ( !is.null(startdate) ) {
    if ( is.numeric(startdate) || is.character(startdate) ) {
      startdate <- parseDatetime(startdate, timezone = timezone)
    } else if ( lubridate::is.POSIXct(startdate) ) {
      startdate <- lubridate::force_tz(startdate, tzone = timezone)
    } else if ( !is.null(startdate) ) {
      stop(paste0(
        "Required parameter 'startdate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }
  
  # handle various enddates
  if ( !is.null(enddate) ) {
    if ( is.numeric(enddate) || is.character(enddate) ) {
      enddate <- parseDatetime(enddate, timezone = timezone)
    } else if ( lubridate::is.POSIXct(enddate) ) {
      enddate <- lubridate::force_tz(enddate, tzone = timezone)
    } else if ( !is.null(enddate) ) {
      stop(paste0(
        "Required parameter 'enddate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }
  
  
  if ( is.null(ylim) ) {
    # Well defined y-axis limits for visual stability
    ylo <- 0
    ymax <- max(dplyr::filter(.data = data, .data$datetime >= startdate & .data$datetime <= enddate)$pm25, na.rm = TRUE)
    if ( ymax <= 50 ) {
      yhi <- 50
    } else if ( ymax <= 100 ) {
      yhi <- 100
    } else if ( ymax <= 200 ) {
      yhi <- 200
    } else if ( ymax <= 400 ) {
      yhi <- 400
    } else if ( ymax <= 600 )  {
      yhi <- 600
    } else if ( ymax <= 1000 )  {
      yhi <- 1000
    } else if ( ymax <= 1500 )  {
      yhi <- 1500
    } else {
      yhi <- 1.05 * ymax
    }
  } else {
    # Standard y-axis limits
    ylo <- ylim[1]
    yhi <- ylim[2]
  }
  
  
  
  # add the scales
  list(
    custom_datetimeScale(startdate, 
                         enddate, 
                         timezone),
    
    scale_y_continuous(limits = c(ylo, yhi),
                       expand = c(0.05,0)),
    ylab("PM2.5 (\u00b5g/m3)"),
    xlab(xlab)
    
    
  )
  
}