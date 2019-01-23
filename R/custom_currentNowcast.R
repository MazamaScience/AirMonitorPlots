#' @title Add a bar for the current NowCast to a plot
#'
#' @description
#' Add a bar for the current NowCast to a plot
#'
#' @param ws_tidy
#' @param monitorID
#' 
#' @import ggplot2
#' @export
#' 
#' @examples 



custom_latestNowcast <- function(ws_tidy,
                                 monitorID = NULL,
                                 timezone = NULL,
                                 width = 0.8) {
  
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
  if ( now - ws_tidy$datetime[lastValidIndex] > lubridate::dhours(3) ) {
    # No Nowcast Available
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
                   color = "gray60")
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
                   label = "current\nNowCast",
                   vjust = 1.5)
  
  
  list(rect,
       rect2,
       text,
       coord_cartesian(clip = "off"))
  
}

if (FALSE) {
  ws_monitor <- airnow_loadLatest()
  tidy <- monitor_toTidy(ws_monitor)
  tidy <- dplyr::filter(tidy, monitorID == "060631010_01")
  startdate <- lubridate::floor_date(lubridate::now() - lubridate::ddays(6), "day")
  enddate <- lubridate::now()
  ggplot_pm25Timeseries(tidy, startdate = startdate, enddate = enddate) +
    stat_dailyAQILevel(timezone = unique(tidy$timezone)) + 
    custom_latestNowcast(tidy)
}