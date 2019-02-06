#' @title PWFSL PM2.5 Diurnal Scales
#'
#' @description
#' Add PWFSL-style x-axis and y-axis scales suitable for a plot showing PM2.5 
#' data as a funciton of hour of the day.
#'
#' @param data pm25 timeseries data. Should match the default dataset of the
#' plot.
#' @param ylim custom y-axis limits. This function will apply a default limit
#' depending on the data. 
#' @param xlab Custom x-axis label. If \code{NULL} a default xlab will be generated. 
#' @param ylab Custam y-axis label. 
#' @param yexp Vector of range expansion constants used to add some padding around the 
#' data on the y-axis, to ensure that they are placed some distance away from the axes.
#' @param xexp Vector of range expansion constants used to add some padding around the 
#' data on the x-axis, to ensure that they are placed some distance away from the axes. 
#' @param offsetBreaks if \code{TRUE}, x-axis ticks and guides are offset by 0.5. 
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
  offsetBreaks = FALSE) {
  
  
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
  
  xmin <- 0 - 23*xexp[1]
  xmax <- 23 + 23*xexp[2]
  breaks <- if (offsetBreaks) seq(-0.5, 22.5, by = 3) else seq(0, 22, by = 3)
  minor_breaks <- if (offsetBreaks) seq(-0.5, 22.5, by = 1) else seq(0, 22, by = 1)
  
  
  
  # add the scales
  list(
    scale_x_continuous(breaks = breaks,
                       minor_breaks = seq(0, 23, by = 1),
                       labels = c("midnight", "3am", "6am", "9am", "Noon", "3pm", "6pm", "9pm"),
                       limits = c(xmin, xmax),
                       expand = c(0,0)),
    scale_y_continuous(limits = c(ylo - yexp[1] * yhi, yhi + yexp[2] * yhi),
                       expand = c(0,0)),
    ylab(ylab),
    xlab(xlab)
    
    
  )
  
}