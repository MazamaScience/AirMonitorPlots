#' @title Create a daily bar plot for a single monitor
#'
#' @description
#' Create a bar plot showing daily average PM2.5 data over a period for the given 
#' monitor. Colored bars represent the PM2.5 readings for each day.
#' 
#' @param ws_monitor \emph{ws_monitor} object containing a single monitor.
#' @param startdate Desired start date (integer or character in Ymd format 
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param colorPalette Palette function to convert monitor values into colors.
#'
#' @return A **ggplot** plot object with a daily bar plot for a single monitor.
#' 
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' startdate <- "2016-08-10"
#' enddate <- "2016-08-17"
#' dailyBarPlotBase(ws_monitor, startdate, enddate)

dailyBarPlotBase <- function(ws_monitor,
                             startdate = NULL,
                             enddate = NULL,
                             colorPalette = aqiPalette("aqi"),
                             title = "") {
  
  # Validate arguments ---------------------------------------------------------
  
  if ( !monitor_isMonitor(ws_monitor) ) {
    stop("Required parameter 'ws_monitor' is not a valid ws_monitor object")
  } else if ( monitor_isEmpty(ws_monitor) ) {
    stop("Required parameter 'ws_monitor' is empty.")
  }
  
  if ( !nrow(ws_monitor$meta) == 1 ) {
    stop("Required parameter 'ws_monitor' must contain only one monitor.")
  }
  
  if ( is.null(startdate) && is.null(enddate) ) {
    stop("Required parameters 'startdate' and/or 'enddate' must be defined.")
  }
  
  # Set up style ---------------------------------------------------------------
  
  # Time limits ----------------------------------------------------------------
  
  timezone <- ws_monitor$meta$timezone[1]
  
  # If a startdate argument was passed, make sure it converts to a valid datetime
  if ( !is.null(startdate) ) {
    if ( is.numeric(startdate) || is.character(startdate) ) {
      startdate <- lubridate::ymd(startdate, tz = timezone)
    } else if ( lubridate::is.POSIXct(startdate) ) {
      startdate <- lubridate::force_tz(startdate, tzone = timezone)
    } else if ( !is.null(startdate) ) {
      stop(paste0(
        "Required parameter 'startdate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }
  
  # If an enddate argument was passed, make sure it converts to a valid datetime
  if ( !is.null(enddate) ) {
    if ( is.numeric(enddate) || is.character(enddate) ) {
      enddate <- lubridate::ymd(enddate, tz = timezone)
    } else if ( lubridate::is.POSIXct(enddate) ) {
      enddate <- lubridate::force_tz(enddate, tzone = timezone)
    } else if ( !is.null(enddate) ) {
      stop(paste0(
        "Required parameter 'enddate' must be integer or character",
        " in Ymd format or of class POSIXct."))
    }
  }
  
  if ( !is.null(startdate) && is.null(enddate) ) {
    enddate <- startdate + lubridate::dhours(23)
  } else if ( is.null(startdate) && !is.null(enddate) ) {
    startdate <- enddate
    enddate <- enddate + lubridate::dhours(23)
  } else if ( !is.null(startdate) && !is.null(enddate) ) {
    enddate <- enddate + lubridate::dhours(23)
  }
  
  # Subset based on startdate and enddate
  mon <- monitor_subset(ws_monitor, tlim=c(startdate,enddate))
  
  # Solar data -----------------------------------------------------------------
  
  ti <- timeInfo(startdate,
                 mon$meta$longitude,
                 mon$meta$latitude,
                 mon$meta$timezone)
  
  # Barplot data ---------------------------------------------------------------
  
  if (startdate == enddate) {
    stop("'startdate' and 'enddate' cannot be equal")
  }
  
  dailyData <- PWFSLSmoke::monitor_dailyStatistic(mon)$data
  names(dailyData) <- c("datetime", "pm25")
  
  dailyData$color = colorPalette(dailyData$pm25)
  
  # Plot data ------------------------------------------------------------------
  
  dailyBarPlotBase <- ggplot() +
  
    geom_bar(data = dailyData,
             aes(
              x = datetime,
              y = pm25
             ),
             fill = dailyData$color,
             color = dailyData$color,
             stat = "identity")
    
    # Remove all plot decorations
    dailyBarPlotBase <- dailyBarPlotBase +
    theme(panel.background = element_rect(fill = "transparent", color = NA)) +
    theme(plot.background = element_rect(fill = "transparent", color = NA)) +
    theme(panel.grid = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1.0)) + 
    theme(axis.ticks = element_blank()) +
    scale_x_datetime(date_breaks = "days" , date_labels = "%b %y")

  
    # Add plot title
    dailyBarPlotBase <- dailyBarPlotBase +
    ggtitle(title) +
    theme(plot.title = element_text(color = "gray30", size = 20, hjust = 0.5))
  

  return(dailyBarPlotBase)
}





