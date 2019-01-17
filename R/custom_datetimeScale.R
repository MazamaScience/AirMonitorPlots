#' @title Add date scale with custom styling for timezone and daterange
#' 
#' @description Add a date scale and custom formatting for creating 
#' consistent timeseries plots. 
#' 
#' @param startdate Desired axis start date limit, in a format that can be 
#' parsed with \link{parseDatetime}.
#' @param enddate Desired axis end date limit, in a format that can be 
#' parsed with \link{parseDatetime}.
#' @param timezone Timezone for label formatting. 
#' @param expand Vector of range expansion constants used to add some padding 
#' around the data, to ensure that they are placed some distance away from the 
#' axes. 
#' @param breaks Custom breaks. If NULL, suitable breaks are calculated.
#' @param minor_breaks Custom minor breaks. If NULL, suitable breaks are
#' calculated. 
#' @param date_labels date format string for formatting date labels.
#' 
#' 
#' @export
#' @import ggplot2
#' 


custom_datetimeScale <- function(startdate = NULL, 
                                 enddate = NULL,
                                 timezone = NULL,
                                 expand = c(0,0.05),
                                 breaks = NULL,
                                 minor_breaks = NULL,
                                 date_labels = "%b %d",
                                 ...) {
  
  
  # TODO:  handle NULL startdate and enddate 
  
  
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
  
  # We will include the complete 'enddate' day
  dayCount <- as.integer(difftime(enddate, startdate, units = "days")) + 1
  
  # Choose date_breaks and minor_breaks
  s <- startdate
  e <- enddate + lubridate::ddays(1) # full 24 hours of enddate
  if ( dayCount >= 0 && dayCount <= 9 ) {
    breaks <- seq(s, e, by = "1 day")
    minor_breaks <- seq(s, e, by = "3 hours")
  } else if ( dayCount <= 21 ) {
    breaks <- seq(s, e, by = "3 days")
    minor_breaks <- seq(s, e, by = "6 hours")
  } else if ( dayCount <= 60 ) {
    breaks <- seq(s, e, by = "1 week")
    minor_breaks <- seq(s, e, by = "1 day")
  } else if ( dayCount <= 120 ) {
    breaks <- seq(s, e, by = "2 weeks")
    minor_breaks <- seq(s, e, by = "1 day")
  } else {
    breaks <- seq(s, e, by = "1 month")
    minor_breaks <- seq(s, e, by = "1 week")
  }
  
  
  # NOTE:  X-axis must be extended to fit the complete last day.
  # NOTE:  Then a little bit more for style.
  xRangeSecs <- as.numeric(difftime(enddate, startdate, timezone, units = "secs"))
  marginSecs <- 0.02 * xRangeSecs
  xlo <- startdate - lubridate::dseconds(marginSecs)
  xhi <- enddate + lubridate::ddays(1) + lubridate::dseconds(marginSecs)
  
  args <- list(...)
  args$expand <- ifelse( is.null(args$expand), c(0,0.05), args$expand )
  args$date_labels <- ifelse( is.null(args$date_labels), "%b %d", args$date_labels)
  
  # Add x-axis
  list(
    scale_x_datetime(
      limits = c(xlo,xhi),
      expand = expand,
      breaks = breaks,
      minor_breaks = minor_breaks,
      date_labels = date_labels,
      timezone = timezone
    ),
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





