#' @title Create a Clock plot for a single monitor
#'
#' @description
#' Create a "clock plot" showing PM2.5 data for a single day for the given 
#' monitors. A colored bar curves around in a clockwise manner with 12/4 of the
#' bar colored for each hour of the local time day.
#' 
#' @param ws_monitor \emph{ws_monitor} object.
#' @param monitorID Monitor ID of interest.
#' @param startdate Desired start date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param style Plot style one of \code{base|icon|icon_fan|full|full_fan}.
#' @param centerColor Color used for the center of the circle.
#'
#' @return A **ggplot** plot object with a "clock plot" for a single monitor.
#'
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' monitorID <- ws_monitor$meta$monitorID[1]
#' startdate <- "2016-08-07"
#' clockPlot(ws_monitor, monitorID, startdate, style = "icon")


clockPlot <- function(ws_monitor,
                      monitorID = NULL,
                      startdate = NULL,
                      enddate = NULL,
                      style = "icon",
                      centerColor = "black") {
  
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Carmel Valley
    ws_monitor <- PWFSLSmoke::Carmel_Valley
    monitorID <- "060530002_01"
    startdate <- "2016-08-07"
    enddate <- NULL
    style <- "icon"
    centerColor <- "black"

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
    if ( is.null(monitorID) ) {
      stop("Argument 'monitorID' must be defined.")
    } else if ( !monitorID %in% ws_monitor$meta$monitorID ) {
      stop(paste0("Monitor ", monitorID, " is not found in 'ws_monitor'"))
    }
    ws_monitor <- monitor_subset(ws_monitor, monitorIDs = monitorID)
  }
  
  # TODO: Accept any variation of style with no required order
  # For example, these should all be valid:
  #
  # "base"
  # "icon"
  # "icon_fan"
  # "fan_icon"
  # "full"
  # "full_fan"
  # "fan_full"
  # "full_avg"
  # "avg_full"
  # "full_fan_avg"
  # "fan_full_avg"
  # "avg_fan_full"
  # ...etc
  
  validStyles <- c("base", "icon", "icon_fan",
                   "full", "full_fan", "full_avg", "full_fan_avg")
  if ( !is.null(style) && !(style %in% validStyles) ) {
    stop(
      paste0(
        "'", style, "' is not a valid 'style' \n",
        "Please choose from: ", paste0(validStyles, collapse = ", ")
      )
    )
  }
  
  # Set up style ---------------------------------------------------------------
  
  centerTextSize = 16
  
  # Create the plot ------------------------------------------------------------
  
  # clockPlot <- clockPlotBase(ws_monitor,
  #                            monitorID,
  #                            startdate,
  #                            style,
  #                            centerColor = "transparent",
  #                            gapFraction = 1/25,
  #                            plotRadius = 1.2,
  #                            dataRadii = c(0,1)) +
  # 
  # annotate("segment", x = 0, xend = plotRadius, y = sunriseFraction, yend = sunriseFraction, color = "black", size = 1.6) +
  # annotate("text", x = 1.1, y = sunriseFraction, label = sunriseText, color = "slategray", size = 4) +
  # annotate("segment", x = 0, xend = plotRadius, y = sunsetFraction, yend = sunsetFraction, color = "black", size = 1.6) +
  # annotate("text", x = 1.1, y = sunsetFraction, label = sunsetText, color = "slategray", size = 4) +
  # 
  # # filled center
  # geom_rect(
  #   aes(
  #     ymin = 0.0,
  #     ymax = 1.0,
  #     xmin = 0.0,
  #     xmax = 0.4),
  #   fill = "red") +
  # 
  # annotate("text", x = 0, y = .5, label = dailyMean, color = "black", size = 16) +
  # # annotate("text", x = 0.85, y = .5, label = paste0(startdate, ", ", mon$meta$siteName), color = "black", size = 5) +
  # 
  # ggtitle(paste0(startdate, ", ", mon$meta$siteName)) +
  # 
  # theme(panel.grid = element_blank(),
  #       axis.title = element_blank(),
  #       axis.text = element_blank(), 
  #       axis.ticks = element_blank(),
  #       legend.position = "none")
  
  if ( stringr::str_detect(style, "base") ) {
    
    clockPlotBase <- clockPlotBase(ws_monitor,
                                   startdate,
                                   enddate,
                                   centerColor = centerColor,
                                   gapFraction = 1/25,
                                   plotRadius = 1,
                                   dataRadii = c(0.5,1),
                                   shadedNight = FALSE,
                                   solarLabels = FALSE)
    
    
  } else if ( stringr::str_detect(style, "icon") ) {
    
    if ( style == "icon" ) {
      plotRadius <- 1.0
      shadedNight <- FALSE
    } else {
      plotRadius <- 1.2
      shadedNight <- TRUE
    }
    
    clockPlotBase <- clockPlotBase(ws_monitor,
                                   startdate,
                                   enddate,
                                   centerColor = centerColor,
                                   gapFraction = 1/25,
                                   plotRadius = plotRadius,
                                   dataRadii = c(0.5,1),
                                   shadedNight = shadedNight,
                                   solarLabels = FALSE)
    
    
  } else if ( stringr::str_detect(style, "full") ) {
    
    if ( style == "full" ) {
      plotRadius <- 1.2
      shadedNight <- FALSE
    } else {
      plotRadius <- 1.2
      shadedNight <- TRUE
    }
    
    clockPlotBase <- clockPlotBase(ws_monitor,
                                   startdate,
                                   enddate,
                                   centerColor = centerColor,
                                   gapFraction = 1/25,
                                   plotRadius = plotRadius,
                                   dataRadii = c(0.5,1),
                                   shadedNight = shadedNight,
                                   solarLabels = TRUE)
    
    
  }
  
  # Add a colored circle with last daily mean
  if ( stringr::str_detect(style, "avg") ) {
    
    dailyMean <- monitor_getDailyMean(ws_monitor, startdate = startdate, enddate = enddate)
    
    clockPlotBase <- clockPlotBase +

      # colored center (slightly smaller than filled center)
      geom_rect(
        aes(
          xmin = 0.0,
          xmax = 0.45,
          ymin = 0.0,
          ymax = 1.0
        ),
        fill = aqiPalette("aqi")(dailyMean)) +
      
      annotate("text", x = 0, y = .5,
               label = round(dailyMean, digits=0),
               color = "black", size = centerTextSize)
      
      
  }
  
  return(clockPlotBase)
  
}
