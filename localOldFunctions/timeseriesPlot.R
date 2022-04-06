#' @title Create a timeseries plot for a single monitor
#'
#' @description
#' Create a  timeseries plot of PM2.5 values at a location.
#' 
#' This function presents a simplified interface to 
#' \code{\link{timeseriesPlotBase}} and collects common options into a set of 
#' named styles.
#' 
#' Current styles include:
#' \itemize{
#' \item{\code{pwfsl} -- used in the PWFSL monitoring site. Best for 1-2
#' weeks of data.}
#' }
#' 
#' The returned object may be further amended with **ggplot** elements.
#' 
#' @details Daily averages are calculated from midnight-to-midnight in monitor 
#' local time.
#' 
#' @param mts_monitor \emph{mts_monitor} object.
#' @param startdate Desired start date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param style Plot style.
#' @param aqiStyle AQI style to add AQI color bars, lines and labels.
#' @param deviceDeploymentID Monitor ID of interest. Required if \code{mts_monitor} 
#' contains more than one monitor.
#' @param title Optional title for the plot.
#'
#' @return A `ggplot` plot object with a "daily barplot" for a single monitor.
#'
#' @seealso \code{\link{timeseriesPlotBase}}
#' 
#' @export
#' @examples
#' NW <- AirMonitor::Northwest_Megafires
#' timeseriesPlot(NW, "2015-08-01", "2015-08-14",
#'                style = "pwfsl",
#'                deviceDeploymentID = "160690014_01",
#'                title = "Daily Average PM2.5\nRuebens, Idaho")

timeseriesPlot <- function(mts_monitor,
                           startdate = NULL,
                           enddate = NULL,
                           style = NULL,
                           aqiStyle = NULL,
                           deviceDeploymentID = NULL,
                           title = "") {
  
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Ruebens, ID
    mts_monitor <- monitor_loadLatest()
    startdate <- "2018-10-30"
    enddate <- "2018-11-05"
    style <- "pwfsl"
    aqiStyle <- "bars_lines"
    deviceDeploymentID <- "060290014_01"
    title <- "Hourly PM2.5 Values and NowCast\nSite: Bakersfield - California Ave"

    timeseriesPlot(mts_monitor, "2018-10-30", "2018-11-05",
                   style = "pwfsl",
                   deviceDeploymentID = "060290014_01",
                   title = "Hourly PM2.5 Values and NowCast\nSite: Bakersfield - California Ave")   
    
  }
  
  # Validate arguments ---------------------------------------------------------
  
  # mts_monitor
  if ( !monitor_isValid(mts_monitor) ) {
    stop("Argument 'mts_monitor' is not a valid mts_monitor object")
  } else if ( monitor_isEmpty(mts_monitor) ) {
    stop("Argument 'mts_monitor' is empty.")
  }
  
  if ( nrow(mts_monitor$meta) == 1 ) {
    deviceDeploymentID <- mts_monitor$meta$deviceDeploymentID[1]
  } else {
    if ( is.null(deviceDeploymentID) ) {
      stop("Argument 'deviceDeploymentID' must be defined.")
    } else if ( !deviceDeploymentID %in% mts_monitor$meta$deviceDeploymentID ) {
      stop(paste0("Monitor ", deviceDeploymentID, " is not found in 'mts_monitor'"))
    }
    mts_monitor <- monitor_subset(mts_monitor, deviceDeploymentIDs = deviceDeploymentID)
  }
  
  # startdate
  if ( is.null(startdate) ) {
    startdate <- lubridate::floor_date(min(mts_monitor$data$datetime),
                                       unit = "days")
  }
  
  # enddate 
  if ( is.null(enddate) ) {
    enddate <- lubridate::floor_date(max(mts_monitor$data$datetime),
                                     unit = "days")
  }
  
  # style
  validStyles <- c("pwfsl")
  if ( !is.null(style) ) {
    if ( !style %in% validStyles ) {
      stop(
        paste0(
          "Invalid style: \"", style, "\". ",
          "The following styles are supported: \"", 
          paste0(validStyles, sep = "|"), "\""
        )
      )
    }
  }
  
  if ( is.null(aqiStyle) ) {
    # Default to no aqiStyle
    aqiStyle <- ""
  } else {
    # Accept any variation of style options with no required order
    validAqiStyleOptions <- c("bars", "lines")
    aqiStyleOptions <- unlist(stringr::str_split(aqiStyle, "_"))
    
    for ( option in aqiStyleOptions ) {
      if ( !option %in% validAqiStyleOptions ) {
        stop(
          paste0(
            "Invalid style option: \"", option, "\". ",
            "The following 'aqiStyle' argument options are supported: \"", 
            paste0(validAqiStyleOptions, collapse = "|"), "\""
          )
        )
      }
    }
  } 
  
  # Time limits ----------------------------------------------------------------
  
  timezone <- mts_monitor$meta$timezone[1]
  
  # handle various startdates
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
  
  # handle various enddates
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
  
  # We will include the complete 'enddate' day
  dayCount <- as.integer(difftime(enddate, startdate, units = "days")) + 1
  
  # Set up style ---------------------------------------------------------------
  
  base_size <- 11
  
  colorPalette <- aqiPalette("aqi")
  ylimStyle <- "auto"
  borderColor <- "black"
  borderSize <- 0.5
  dateFormat <- "%b %d"
  
  if ( style == "pwfsl" ) {
    
    ylimStyle <- "pwfsl" # well defined limits for visual stability
    aqiStyle <- ifelse(aqiStyle == "", "bars_lines", aqiStyle)
    
  } else if ( style == "week" ) {
    
    # No changes
    
  } else if ( style == "month" ) {
    
    borderColor <- "transparent" # lots of lines leave no room for borders
    
  }
  
  # Create the plot ------------------------------------------------------------
  
  ggplotBase <- timeseriesPlotBase(
    mts_monitor,
    startdate,
    enddate,
    colorPalette = colorPalette,
    ylimStyle = ylimStyle,
    aqiStyle = aqiStyle,
    dateFormat = dateFormat,
    title = title
  )
  
  # Apply the appropriate theme ------------------------------------------------
  
  if ( style == "pwfsl" ) {
    
    ggplotBase <- ggplotBase + 
      theme_timeseriesPlot_pwfsl()
    
  } else {
    
    ggplotBase <- ggplotBase + 
      theme_timeseriesPlot_pwfsl()
    
  }
  
  # Additional data-dependent theming ------------------------------------------
  
  # Remove the Y-axis line if AQI stacked bars are shown
  if ( "bars" %in% aqiStyle ) {
    ggplotBase <- ggplotBase +
      theme(
        axis.line.y = element_blank()
      )
  }
  
  # Tilt X-axis labels and add tick marks for > 7 days
  if ( dayCount > 7 )
    ggplotBase <- ggplotBase +
    theme(
      ###axis.ticks.x = element_line(),
      axis.text.x = element_text(
        size = 1.0 * base_size,
        margin = margin(t = 0.50 * base_size),
        angle = 45,
        hjust = 1
      )
    )
  
  # Return ---------------------------------------------------------------------
  
  return(ggplotBase)
  
}
