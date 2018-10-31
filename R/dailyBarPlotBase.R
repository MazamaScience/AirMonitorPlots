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
#' startdate <- "2016-08-5"
#' enddate <- "2016-08-19"
#' dailyBarPlotBase(ws_monitor, startdate, enddate)

dailyBarPlotBase <- function(ws_monitor,
                             startdate = NULL,
                             enddate = NULL,
                             colorPalette = aqiPalette("aqi"),
                             title = "",
                             yLimits = NULL,
                             barOutlineColor = "black",
                             barOutlineSize = 1.0) {
  
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
  
  plotTitleSize = 20
  xAxisTextSize <- 10
  
  tickPeriod = "days"
  dayDiff <- difftime(enddate, startdate, units = "days")
  if (dayDiff >= 0 && dayDiff <= 7) {
    tickPeriod = "days"
  } else if (dayDiff <= 21) {
    tickPeriod = "3 days"
  } else if (dayDiff <= 60) {
    tickPeriod = "weeks"
  } else {
    tickPeriod = "months"
  }
  
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
  
  # AQIBar data -----------------------------------------------------------------
  
  AQIBar <- data.frame(PWFSLSmoke::AQI$breaks_24)
  names(AQIBar) <- c("aqi")
  
  # Barplot data ---------------------------------------------------------------
  
  if ( startdate == enddate ) {
    stop("'startdate' and 'enddate' cannot be equal")
  }
  
  dailyData <- PWFSLSmoke::monitor_dailyStatistic(mon)$data
  names(dailyData) <- c("datetime", "pm25")
  dailyData$color = colorPalette(dailyData$pm25)
  
  if ( any(is.na(dailyData$pm25)) ) {
    stop("Missing readings inside date range")
  }
  
  if (is.null(yLimits) ) {
    yLimits = c(0, max(dailyData$pm25, na.rm = TRUE))
  }
  
  # Plot data ------------------------------------------------------------------
  
  dailyBarPlotBase <- ggplot() +
  
    # Add daily statistic bars
    geom_bar(data = dailyData,
             aes(
              x = datetime,
              y = pm25
             ),
             fill = dailyData$color,
             color = barOutlineColor,
             stat = "identity")
    
    # Remove plot decorations
    dailyBarPlotBase <- dailyBarPlotBase +
    theme(panel.background = element_rect(fill = "transparent", color = NA)) +
    theme(plot.background = element_rect(fill = "transparent", color = NA)) +
    theme(panel.grid = element_blank()) +
    theme(axis.title.x = element_blank())
    
    # Style text and tickmarks
    dailyBarPlotBase <- dailyBarPlotBase +
    theme(axis.text.x = element_text(size = xAxisTextSize, angle = 45, vjust = 0.5)) + 
    scale_x_datetime(date_breaks = tickPeriod, date_labels = "%b %d") +
    ylab("PM2.5 (μg/m3)") +
    ylim(yLimits)
    
    # Add plot title
    dailyBarPlotBase <- dailyBarPlotBase +
    ggtitle(title) +
    theme(plot.title = element_text(color = "gray30", size = plotTitleSize, hjust = 0.5))

  return(dailyBarPlotBase)
}





