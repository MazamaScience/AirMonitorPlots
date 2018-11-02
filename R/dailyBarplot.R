#' @title Create a daily barplot for a single monitor
#'
#' @description
#' Create a daily average barplot of PM2.5 values at a location.
#' 
#' This function presents a simplified interface to \code{\link{dailyBarplotBase}}
#' and collects common options into a set of named styles.
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
#' @param ws_monitor \emph{ws_monitor} object.
#' @param startdate Desired start date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param style Plot style.
#' @param monitorID Monitor ID of interest. Required if \code{ws_monitor} 
#' contains more than one monitor.
#' @param currentNowcast Real-time current Nowcast value -- for use in plots 
#' presented in the PWFSL monitoring site.
#' @param currentPrediction Real-time current prediction for today's daily 
#' average -- for use in plots presented in the PWFSL monitoring site.
#' @param title Optional title for the plot.
#'
#' @return A `ggplot` plot object with a "daily barplot" for a single monitor.
#'
#' @seealso \code{\link{dailyBarplotBase}}
#' 
#' @export
#' @examples
#' NW <- PWFSLSmoke::Northwest_Megafires
#' dailyBarplot(NW, "2015-07-01", "2015-10-01",
#'              style = "month",
#'              monitorID = "160690014_01",
#'              title = "Daily Average PM2.5\nRuebens, Idaho")

dailyBarplot <- function(ws_monitor,
                         startdate = NULL,
                         enddate = NULL,
                         style = NULL,
                         monitorID = NULL,
                         currentNowcast = NULL,
                         currentPrediction = NULL,
                         title = "") {
  
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Ruebens, ID
    ws_monitor <- PWFSLSmoke::Northwest_Megafires
    startdate <- "2015-08-20"
    enddate <- "2015-08-26"
    style <- "pwfsl"
    monitorID <- "160690014_01"
    currentNowcast <- NULL
    currentPrediction <- NULL
    title <- "Ruebens, ID"
    
  }
  
  # Validate arguments ---------------------------------------------------------
  
  # ws_monitor
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
  
  # startdate
  if ( is.null(startdate) ) {
    startdate <- lubridate::floor_date(min(ws_monitor$data$datetime),
                                       unit = "days")
  }
  
  # enddate 
  if ( is.null(enddate) ) {
    enddate <- lubridate::floor_date(max(ws_monitor$data$datetime),
                                     unit = "days")
  }
  
  # style
  validStyles <- c("pwfsl", "week", "month")
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
  
  # Time limits ----------------------------------------------------------------
  
  timezone <- ws_monitor$meta$timezone[1]
  
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
  
  # Auto-style
  if ( is.null(style) ) {
    if ( dayCount <= 21 ) {
      style = "week"
    } else {
      style = "month"
    }
  }
  
  # Set up style ---------------------------------------------------------------
  
  base_size <- 11
  
  colorPalette <- aqiPalette("aqi")
  ylimStyle <- "auto"
  borderColor <- "black"
  borderSize <- 0.5
  showAQIStackedBars <- FALSE
  showAQILines <- FALSE
  showAQILegend <- FALSE
  dateFormat <- "%b %d"
  
  if ( style == "pwfsl" ) {
    
    ylimStyle <- "pwfsl" # well defined limits for visual stability
    showAQIStackedBars <- TRUE
    showAQILines <- TRUE
    
  } else if ( style == "week" ) {
    
    # No changes
    
  } else if ( style == "month" ) {
    
    borderColor <- "transparent" # lots of lines leave no room for borders
    
  }
  
  # Create the plot ------------------------------------------------------------
  
  dailyBarplotBase <- dailyBarplotBase(
    ws_monitor,
    startdate,
    enddate,
    colorPalette = colorPalette,
    ylimStyle = ylimStyle,
    borderColor = borderColor,
    borderSize = borderSize,
    currentNowcast = currentNowcast,
    currentPrediction = currentPrediction,
    showAQIStackedBars = showAQIStackedBars,
    showAQILines = showAQILines,
    showAQILegend = showAQILegend,
    dateFormat = dateFormat,
    title = title
  )
  
  # Apply the appropriate theme ------------------------------------------------
  
  if ( style == "pwfsl" ) {
    
    dailyBarplotBase <- dailyBarplotBase + 
      theme_dailyBarplot_pwfsl()
    
  } else {
    
    dailyBarplotBase <- dailyBarplotBase + 
      theme_dailyBarplot_pwfsl()
    
  }
  
  # Additional data-dependent theming ------------------------------------------
  
  # Remove the Y-axis line 
  if ( showAQIStackedBars ) {
    dailyBarplotBase <- dailyBarplotBase +
      theme(
        axis.line.y = element_blank()
      )
  }
  
  # Tilt X-axis labels and add tick marks for > 7 days
  if ( dayCount > 7 )
    dailyBarplotBase <- dailyBarplotBase +
    theme(
      axis.ticks.x = element_line(),
      axis.text.x = element_text(
        size = 1.0 * base_size,
        margin = margin(t = 0.50 * base_size),
        angle = 45,
        hjust = 1
      )
    )
  
  return(dailyBarplotBase)
  
}
