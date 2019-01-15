pwfsl_scales <- function(data = NULL,
                         startdate = NULL, 
                         enddate = NULL, 
                         ylim = NULL) {
  
  if (is.null(data)) {
    if (is.null(startdate) || is.null(enddate) || is.null(ylim)) {
      stop("at least one of data, startdate, enddate, and ylim must be specified.")
    }
  }
  
  if (is.null(startdate)) {
    startdate <- min(data$datetime)
  }
  if (is.null(enddate)) {
    enddate <- max(data$datetime)
  }
  
  if (length(unique(data$timezone)) > 1) {
    timezone <- "UTC"
  } else {
    timezone <- data$timezone[1]
  }
  
  if ( is.null(ylim) ) {
    # Well defined y-axis limits for visual stability
    ylo <- 0
    ymax <- max(filter(data, datetime >= startdate && datetime <= enddate)$pm25, na.rm = TRUE)
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
    ylo <- 0
    yhi <- max(1.00*data$pm25, na.rm = TRUE)
  }
  
  
  
  # add the scales
  list(
    scale_x_pwfslDate(startdate, 
                      enddate, 
                      timezone),
    
    scale_y_continuous(limits = c(ylo, yhi),
                       expand = c(0.05,0)),
    
    if ( dayCount > 7 ) {
      theme(
        ###axis.ticks.x = element_line(),
        axis.text.x = element_text(
          size = 1.0 * 11,
          margin = margin(t = 0.50 * 11),
          angle = 45,
          hjust = 1
        )
      )
    }
    
  )
  
}