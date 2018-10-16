#' @title Create a Clock plot for a single monitor
#'
#' @description
#' Create a "clock plot" showing PM2.5 data for a single day for the given 
#' monitors. A colored bar curves around in a clockwise manner with 12/4 of the
#' bar colored for each hour of the local time day.
#' 
#' @param ws_monitor \emph{ws_monitor} object.
#' @param startdate Desired start date (integer or character in Ymd format).
#' @param centerColor Color used for the center of the circle.
#' @param gapFraction Fraction of the circle used as the day boundary gap.
#' @param plotRadius Full radius of the plot. 
#' @param dataRadii Inner and outer radii for the data portion of the plot [0:1]. 
#' @param shadedNight Add nighttime shading.
#' @param solarLabels Add sunrise/sunset labels.
#'
#' @return A **ggplot** plot object with a "clock plot" for a single monitor.
#' 
#' Room for annotations can be created by setting \code{plotRadius = 1.2}.
#'
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' startdate <- "2016-08-07"
#' clockPlotBase(ws_monitor, startdate)


clockPlotBase <- function(ws_monitor,
                          startdate = NULL,
                          centerColor = "black",
                          gapFraction = 1/25,
                          plotRadius = 1.0,
                          dataRadii = c(0.5,1.0),
                          shadedNight = FALSE,
                          solarLabels = FALSE) {
  
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Carmel Valley
    ws_monitor <- PWFSLSmoke::Carmel_Valley
    startdate <- "2016-08-07"
    centerColor <- "black"
    gapFraction <- 1/25
    plotRadius <- 1.2
    dataRadii <- c(0.5, 1.0)
    shadedNight <- TRUE
    solarLabels <- FALSE
    centerAvg <- FALSE
    
  }
  
  # Validate arguments ---------------------------------------------------------
  
  if ( !monitor_isMonitor(ws_monitor) ) {
    stop("Argument 'ws_monitor' is not a valid ws_monitor object")
  } else if ( monitor_isEmpty(ws_monitor) ) {
    stop("Argument 'ws_monitor' is empty.")
  }
  
  if ( nrow(ws_monitor$meta) == 1 ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  } else {
    stop("Argument 'ws_monitor' must contain only one monitor.")
  }
  
  # Set up style ---------------------------------------------------------------
  
  colorPalette <- aqiPalette("aqi")
  
  if ( shadedNight ) {
    shadedNightColor <- adjustcolor("black", 0.1)
  } else {
    shadedNightColor <- "transparent"
  }
  
  solarTickColor <- "gray50"
  solarTickSize <- 1
  solarLabelColor <- "black"
  solarLabelSize <- 4
  
  
  # Set up data ----------------------------------------------------------------
  
  # TODO:  Allow for time chunking (3 or 4 hr chunks with uniform values) or
  # TODO:  smoothing (using monitor_nowcast()?)
  
  # Subset based on monitorID
  
  mon <- monitor_subset(ws_monitor, monitorIDs = monitorID)
  
  # Subset based on startdate
  
  timezone <- mon$meta$timezone[1]
  
  if ( is.numeric(startdate) || is.character(startdate) ) {
    startdate <- lubridate::ymd(startdate, tz = timezone)
  } else if ( lubridate::is.POSIXct(startdate) ) {
    startdate <- lubridate::force_tz(startdate, tzone = timezone)
  } else if ( !is.null(startdate) ) {
    stop(paste0(
      "Argument 'startdate' must be a numeric/charcter vector",
      " of the form yyyymmdd or of class POSIXct."))
  }
  enddate <- startdate + lubridate::dhours(23)
  
  mon <- monitor_subset(mon, tlim=c(startdate,enddate))
  
  dailyMean <- round(mean(mon$data[,2], na.rm = TRUE), digits = 0)
  
  ti <- timeInfo(startdate,
                 mon$meta$longitude,
                 mon$meta$latitude,
                 mon$meta$timezone)
  
  # Formatting the sunrise and sunset time of day
  sunriseHours <- as.numeric(difftime(ti$sunrise, startdate, units = "hours"))
  sunriseFraction <- sunriseHours * (1 - gapFraction) / 24
  sunsetHours <- as.numeric(difftime(ti$sunset, startdate, units = "hours"))
  sunsetFraction <- sunsetHours * (1 - gapFraction) / 24
  
  clockData <- mon$data
  names(clockData) <- c("datetime", "pm25")
  
  # Define the start, end, and color of each period
  clockData$fraction = (1 - gapFraction) / 24
  clockData$ymax = cumsum(clockData$fraction)
  clockData$ymin <- c(0, lag(clockData$ymax)[-1])
  clockData$color = colorPalette(clockData$pm25)
  
  shadedNightData <- data.frame(
    xmin = c(0,0),
    xmax = c(plotRadius,plotRadius),
    ymin = c(0,sunsetFraction),
    ymax = c(sunriseFraction,1.0)
  )
  
  # For bottom gap between the start and end of the day
  thetaOffset <- pi + (2 * pi) * gapFraction / 2
  
  # Set up labels --------------------------------------------------------------
  
  sunriseText <- paste0("Sunrise\n",
                        lubridate::hour(ti$sunrise), ":",
                        lubridate::minute(ti$sunrise))
  sunsetText <- paste0("Sunset\n",
                       lubridate::hour(ti$sunset), ":",
                       lubridate::minute(ti$sunset))
  
  # Plot data ------------------------------------------------------------------
  
  clockPlotBase <- ggplot() +
    
    # filled center
    geom_rect(
      aes(
        xmin = 0.0,
        xmax = 1.0,
        ymin = 0.0,
        ymax = 1.0
      ),
      fill = centerColor) +

    # polar coordinate system
    coord_polar(theta = 'y', direction = 1, start = thetaOffset) +
    xlim(0, plotRadius) +
    ylim(0, 1) + 
  
    # add shaded night
    geom_rect(
      data = shadedNightData,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = shadedNightColor) +
    
    # add colored hour segments
    geom_rect(
      data = clockData,
      aes(
        ymin = ymin,
        ymax = ymax,
        xmin = dataRadii[1],
        xmax = dataRadii[2]),
      fill = clockData$color,
      color = clockData$color)
    
  # solar labels
  if ( solarLabels ) {
    
    clockPlotBase <- clockPlotBase +
      # annotate("segment", x = dataRadii[2], xend = 0.90*plotRadius, 
      #          y = sunriseFraction, yend = sunriseFraction,
      #          color = solarTickColor, size = solarTickSize) +
      annotate("text", x = 1.0*plotRadius, y = sunriseFraction,
               label = sunriseText,
               color = solarLabelColor, size = solarLabelSize) +
      # annotate("segment", x = dataRadii[2], xend = 0.90*plotRadius, 
      #          y = sunsetFraction, yend = sunsetFraction, 
      #          color = solarTickColor, size = solarTickSize) +
      annotate("text", x = 1.0*plotRadius, y = sunsetFraction, 
               label = sunsetText,
               color = solarLabelColor, size = solarLabelSize)
      
  }
  
  # Remove all plot decorations
  clockPlotBase <- clockPlotBase +
    theme(panel.background = element_rect(fill = "transparent", colour = NA)) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
    theme(panel.grid = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank())
  
  
  return(clockPlotBase)
  
}
