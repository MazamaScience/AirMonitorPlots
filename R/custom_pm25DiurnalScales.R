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
#' @param xlab Custom xlab. If \code{NULL} a default xlab will be generated. 
#' @param yexp Vector of range expansion constants used to add some padding around the 
#' data on the y-axis, to ensure that they are placed some distance away from the axes.
#' @param xexp Vector of range expansion constants used to add some padding around the 
#' data on the x-axis, to ensure that they are placed some distance away from the axes. 
#' 
#' @importFrom rlang .data
#' @import ggplot2
#' @export
#' 

custom_pm25DiurnalScales <- function(
  data = NULL,
  ylim = NULL,
  xlab = NULL,
  ylab = "PM2.5 (\u00b5g/m3)",
  yexp = c(0.05, 0),
  xexp = c(0, 0.05)) {
  
  
  if ( monitor_isMonitor(data) ) {
    data <- monitor_toTidy(data)
  } else if ( monitor_isTidy(data) ) {
    data <- data
  } else {
    stop("data must be either a ws_monitor object or ws_tidy object.")
  }
  
  
  
  if ( is.null(ylim) ) {
    # Well defined y-axis limits for visual stability
    ylo <- 0
    ymax <- max(data$pm25, na.rm = TRUE)
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
    scale_x_continuous(breaks = seq(0, 23, by = 3),
                       minor_breaks = seq(0, 23, by = 1),
                       labels = c("midnight", "3am", "6am", "9am", "noon", "3pm", "6pm", "9pm")),
    scale_y_continuous(limits = c(ylo, yhi),
                       expand = yexp),
    ylab(ylab),
    xlab(xlab)
    
    
  )
  
}