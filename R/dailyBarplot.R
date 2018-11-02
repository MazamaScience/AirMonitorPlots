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
#' \item{\code{monitoring} -- used in the PWFSL monitoring site. Best for 1-2
#' weeks of data.}
#' }
#' 
#' The returned object may be further amended with **ggplot** elements.
#' 
#' @details Daily averages are calculated from midnight-to-midnight in monitor 
#' local time.
#' 
#' @param ws_monitor \emph{ws_monitor} object.
#' @param monitorID Monitor ID of interest.
#' @param startdate Desired start date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param style Plot style.
#' @param currentNowcast Real-time current Nowcast value -- for use in plots 
#' presented in the PWFSL monitoring site.
#' @param currentPrediction Real-time current prediction for today's daily 
#' average -- for use in plots presented in the PWFSL monitoring site.
#' @param title Optional title for the plot.
#'
#' @return A **ggplot** plot object with a "daily barplot" for a single monitor.
#'
#' @seealso \code{\link{dailyBarplotBase}}
#' 
#' @export
#' @examples
#' mon <- PWFSLSmoke::Carmel_Valley
#' id <- mon$meta$monitorID[1]
#' start1 <- "2016-06-01"
#' start2 <- "2016-07-01"
#' start3 <- "2016-08-01"
#' end1 <- "2016-08-08"
#' end2 <- "2016-08-15"
#' end3 <- "2016-08-25"
#' dbp1 <- dailyBarplot(mon, id, start3, end1, "monitoring",
#'                      title = "Carmel Valley\n2016")
#' dbp2 <- dailyBarplot(mon, id, start3, end2, "monitoring",
#'                      title = "Carmel Valley\n2016")
#' dbp3 <- dailyBarplot(mon, id, start2, end2, "monitoring",
#'                      title = "Carmel Valley\n2016")
#' dbp4 <- dailyBarplot(mon, id, start1, end3, "monitoring",
#'                      title = "Carmel Valley\n2016")
#' gridExtra::grid.arrange(dbp1, dbp2, dbp3, dbp4, nrow = 2)

dailyBarplot <- function(ws_monitor,
                         monitorID = NULL,
                         startdate = NULL,
                         enddate = NULL,
                         style = "monitoring",
                         currentNowcast = NULL,
                         currentPrediction = NULL,
                         title = "") {
  
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Carmel Valley
    ws_monitor <- PWFSLSmoke::Carmel_Valley
    monitorID <- "060530002_01"
    startdate <- "2016-08-01"
    enddate <- "2016-08-15"
    style <- "monitoring"
    title <- "Carmel Valley"
    
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
  
  # Accept any variation of style options with no required order
  validStyles <- c("monitoring")
  
  if ( !style %in% validStyles ) {
    stop(
      paste0(
        "Invalid style: \"", style, "\". ",
        "The following styles are supported: \"", 
        paste0(validStyles, sep = "|"), "\""
      )
    )
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

  dayCount <- as.integer(difftime(enddate, startdate, units = "days"))
  
  # Set up style ---------------------------------------------------------------
  
  base_size <- 11
  
  if ( style == "monitoring" ) {
    
    colorPalette <- aqiPalette("aqi")
    ylim <- NULL
    borderColor <- "black"
    borderSize <- 0.5

  }
  
  # Create the plot ------------------------------------------------------------
  
  dailyBarplotBase <- dailyBarplotBase(
    ws_monitor,
    startdate,
    enddate,
    colorPalette = colorPalette,
    ylim = ylim,
    borderColor = borderColor,
    borderSize = borderSize,
    currentNowcast = currentNowcast,
    currentPrediction = currentPrediction,
    title = title
  )
  
  # Apply the appropriate theme ------------------------------------------------
  
  if ( style == "monitoring" ) {
    
    dailyBarplotBase <- dailyBarplotBase + 
      theme_dailyBarplot_monitoring()
    
  }
  
  # Additional data-dependent theming ------------------------------------------
  
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
