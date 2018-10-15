#' @title Create a Clock plot for a single monitor
#'
#' @description
#' Create a "clock plot" showing PM2.5 data for a single day for the given 
#' monitors. A colored bar curves around in a clockwise manner with 12/4 of the
#' bar colored for each hour of the local time day.
#' 
#' @param ws_monitor \emph{ws_monitor} object.
#' @param monitorID Monitor ID of interest.
#' @param startdate Desired start date (integer or character in Ymd format).
#' @param style Plot style one of \code{base|icon}.
#' @param centerColor Color used for the center of the circle.
#' @param gapFraction Fraction of the circle used as the day boundary gap.
#' @param dataRadii Inner and outer radii for the data portion of the plot [0:1]. 
#'
#' @return A **ggplot** plot object with a "clock plot" for a single monitor.
#'
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' monitorID <- ws_monitor$meta$monitorID[1]
#' startdate <- "2016-08-07"
#' clockPlotBase(ws_monitor, monitorID, startdate, "icon")


clockPlotBase <- function(ws_monitor,
                          monitorID = NULL,
                          startdate = NULL,
                          style = 'icon',
                          centerColor = "black",
                          gapFraction = 1/25,
                          dataRadii = c(0.5,1.0)) {
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Carmel Valley
    ws_monitor <- PWFSLSmoke::Carmel_Valley
    monitorID <- "060530002_01"
    startdate <- "2016-08-07"
    style <- "icon"
    centerColor <- "black"
    gapFraction <- 1/25
    dataRadii <- c(0.3, 1.0)
    
  }
  
  # Validate arguments ---------------------------------------------------------
  
  if ( !monitor_isMonitor(ws_monitor) ) {
    stop("Argument 'ws_monitor' is not a valid ws_monitor object")
  } else if ( monitor_isEmpty(ws_monitor) ) {
    stop("Argument 'ws_monitor' is empty.")
  }
  
  if ( nrow(ws_monitor$meta == 1) ) {
    monitorID <- ws_monitor$meta$monitorID[1]
  } else {
    if ( is.null(monitorID) ) {
      stop("Argument 'monitorID' must be defined.")
    } else if ( !monitorID %in% ws_monitor$meta$monitorID ) {
      stop(paste0("Monitor ", monitorID, " is not found in 'ws_monitor'"))
    }
  }
  
  validStyles <- c("base", "icon")
  if ( !is.null(style) && !(style %in% validStyles) ) {
    stop(
      paste0(
        "'", style, "' is not a valid 'style' \n",
        "Please choose from: ", paste0(validStyles, collapse = ", ")
      )
    )
  }
  
  # Set up data ----------------------------------------------------------------
  
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
  
  # Transform data
  
  # TODO:  Allow for time chunking (3 or 4 hr chunks with uniform values) or
  # TODO:  smoothing (using monitor_nowcast()?)
  
  # Set up labels --------------------------------------------------------------
  
  # TODO:  Anything relevant for this section? (see dailyHourlyBarplot)
  
  # Define scales --------------------------------------------------------------
  
  # TODO:  Anything relevant for this section? (see dailyHourlyBarplot)
  
  # Plot data ------------------------------------------------------------------
  
  colorPalette <- aqiPalette("aqi")
  
  clockData <- mon$data
  names(clockData) <- c("datetime", "pm25")
  
  # Define the start, end, and color of each period
  clockData$fraction = (1 - gapFraction) / 24
  clockData$ymax = cumsum(clockData$fraction)
  clockData$ymin <- c(0, lag(clockData$ymax)[-1])
  clockData$color = colorPalette(clockData$pm25)
  
  # Filled center is just a single rectangle
  filledCenterData <- clockData[1,]
  filledCenterData$ymax <- 1
  filledCenterData$color <- centerColor
  
  # For bottom gap between the start and end of the day
  thetaOffset <- pi + (2 * pi) * gapFraction / 2
  
  # Create the plot ------------------------------------------------------------
  
  clockPlotBase <- ggplot(clockData) +
    
    # filled center
    geom_rect(
      data = filledCenterData,
      aes(
        ymin = 0.0, 
        ymax = 1.0, 
        xmin = 0.0, 
        xmax = 1.0),
      fill = centerColor) +
    
    # colored hour segments
    geom_rect(
      aes(
        ymin = ymin, 
        ymax = ymax, 
        xmin = dataRadii[1], 
        xmax = dataRadii[2]),
      fill = clockData$color,
      color = clockData$color) +
    
    # polar coordinate system
    coord_polar(theta = 'y', direction = 1, start = thetaOffset) +
    xlim(0, 1) +
    ylim(0, 1)
  
  if ( style == "icon" ) {
    
    # Remove all plot decorations
    clockPlotBase <- clockPlotBase + 
      theme(panel.background = element_rect(fill = "transparent", colour = NA)) +
      theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
      theme(panel.grid = element_blank()) +
      theme(axis.title = element_blank()) +
      theme(axis.text = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(legend.position = "none")
    
  }
  
  return(clockPlotBase)
  
}
