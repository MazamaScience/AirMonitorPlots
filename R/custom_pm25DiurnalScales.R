#' @title PWFSL PM2.5 timeseries scales
#'
#' @description
#' Add PWFSL-style x-axis and y-axis scales suitable for a timeseries plot 
#' showing PM2.5 data. 
#'
#' @param data pm25 timeseries data. Should match the default dataset of the
#' plot
#' @param ylim custom y-axis limits. This function will apply a default limit
#' depending on the data. 
#' @param xlab Custom x-axis label. If \code{NULL} a default xlab will be generated. 
#' @param ylab Custam y-axis label. 
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
  yexp = c(0.05, 0.05),
  xexp = c(0.05, 0.05),
  offsetBreaks = FALSE,
  hr24 = FALSE) {
  
  
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
  
  xhi <- ifelse(hr24, 24, 23)
  xmin <- 0 - xhi*xexp[1]
  xmax <- xhi + xhi*xexp[2]
  breaks <- if (offsetBreaks) seq(-0.5, xhi - 0.05, by = 3) else seq(0, xhi, by = 3)
  minor_breaks <- if (offsetBreaks) seq(-0.05, xhi - 0.05, by = 1) else seq(0, xhi, by = 1)
  
  labels <- c("midnight", "3am", "6am", "9am", "Noon", "3pm", "6pm", "9pm")
  if (hr24) labels <- c(labels, "")
  
  
  # add the scales
  list(
    scale_x_continuous(breaks = breaks,
                       minor_breaks = seq(0, 23, by = 1),
                       labels = labels,
                       limits = c(xmin, xmax),
                       expand = c(0,0)),
    scale_y_continuous(limits = c(ylo - yexp[1] * ymax, yhi + yexp[2] * ymax),
                       expand = c(0,0)),
    ylab(ylab),
    xlab(xlab)
    
    
  )
  
}