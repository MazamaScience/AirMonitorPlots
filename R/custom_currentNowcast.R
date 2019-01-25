#' @title Add a bar for the current NowCast to a plot
#'
#' @description
#' Add a bar for the current NowCast to a plot
#'
#' @param ws_tidy \code{ws_tidy} object
#' @param monitorID MonitorID to calculate NowCast for. Required if
#' \code{ws_tidy} represents more than one monitor.
#' @param timezone Timezone
#' @param width Bar width, in fractions of days. 
#' @param text_size Size for the label text
#' @param maxValidLatencyHours Number of hours of missing values before
#' a missing value bar will be printed.
#' @param label Text for the label. 
#' 
#' @import ggplot2
#' @export
#' 



custom_currentNowcast <- function(ws_tidy,
                                 monitorID = NULL,
                                 timezone = NULL,
                                 width = 0.8,
                                 text_size = 4,
                                 maxValidLatencyHours = 3,
                                 label = "Current\nNowCast") {
  
  if ( !is.null(monitorID) ) {
    ws_tidy <- dplyr::filter(.data = ws_tidy, .data$monitorID == !!monitorID)
  }
  if ( length(unique(ws_tidy$monitorID)) > 1 ) {
    stop("Cannot calculate for more than one monitorID")
  }
  
  if (is.null(timezone)) {
    timezone <- unique(ws_tidy$timezone)
  }
  now <- lubridate::now(timezone)
  
  lastValidIndex <- dplyr::last(which(!is.na(ws_tidy$pm25)))
  if ( now - ws_tidy$datetime[lastValidIndex] > lubridate::dhours(maxValidLatencyHours) ) {
    currentNowcast <- 0
  } else {
    nowcast <- .nowcast(ws_tidy$pm25)
    currentNowcast <- nowcast[lastValidIndex]
  }
  center <- lubridate::floor_date(now, "day") + lubridate::dhours(12)
  left <- center - width/2*86400
  right <- center + width/2*86400
  
  color <- AQI$colors[.bincode(currentNowcast, AQI$breaks_24)]
  
  rect <- annotate("rect", 
                   xmin = left, 
                   xmax = right,
                   ymin = 0,
                   ymax = currentNowcast,
                   fill = color,
                   color = "gray60",
                   alpha = .8)
  rect2 <- annotate("rect", 
                    xmin = left, 
                    xmax = right,
                    ymin = 0,
                    ymax = currentNowcast,
                    fill = "gray60",
                    color = "gray60",
                    alpha = .3)
  text <- annotate("text", 
                   y = 0,
                   x = center,
                   label = label,
                   vjust = 1.5,
                   color = "gray40",
                   size = text_size)
  
  
  list(
    rect,
    rect2,
    text,
    coord_cartesian(clip = "off") # Turn off clipping so labels can be added outside of plot region
  )
  
}
