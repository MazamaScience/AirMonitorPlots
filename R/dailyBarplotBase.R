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
#' @param ylimStyle Style of y-axis limits. One of \code{auto|pwfsl}.
#' @param borderColor Border color for individual bars.
#' @param borderSize Border size for individual bars.
#' @param currentNowcast Real-time current Nowcast value -- for use in plots 
#' presented in the PWFSL monitoring site.
#' @param currentPrediction Real-time current prediction for today's daily 
#' average -- for use in plots presented in the PWFSL monitoring site.
#' @param title Optional title.
#'
#' @return A **ggplot** plot object with a daily bar plot for a single monitor.
#'
#' @importFrom rlang .data
#' @export
#' @examples
#' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' startdate <- "2016-08-5"
#' enddate <- "2016-08-19"
#' dailyBarplotBase(ws_monitor, startdate, enddate)

dailyBarplotBase <- function(ws_monitor,
                             startdate = NULL,
                             enddate = NULL,
                             colorPalette = aqiPalette("aqi"),
                             ylimStyle = "auto",
                             borderColor = "black",
                             borderSize = 1.0,
                             currentNowcast = NULL,
                             currentPrediction = NULL,
                             showAQIStackedBars = FALSE,
                             showAQILines = FALSE,
                             showAQILegend = FALSE,
                             title = "") {
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Carmel Valley
    ws_monitor <- PWFSLSmoke::Carmel_Valley
    startdate <- "2016-07-01"
    enddate <- "2016-08-28"
    colorPalette <- aqiPalette("aqi")
    ylimStyle <- "pwfsl"
    borderColor <- "black"
    borderSize <- 1
    currentNowcast <- 52
    currentPrediction <- 48
    showAQIStackedBars <- TRUE
    showAQILines <- TRUE
    showAQILegend <- FALSE
    title <- ""
    
  }
  
  # Validate arguments ---------------------------------------------------------
  
  if ( !monitor_isMonitor(ws_monitor) ) {
    stop("Required parameter 'ws_monitor' is not a valid ws_monitor object.")
  } else if ( monitor_isEmpty(ws_monitor) ) {
    stop("Required parameter 'ws_monitor' is empty.")
  }
  
  if ( !nrow(ws_monitor$meta) == 1 ) {
    stop("Required parameter 'ws_monitor' must contain only one monitor.")
  }
  
  if ( is.null(startdate) && is.null(enddate) ) {
    stop("Required parameters 'startdate' and/or 'enddate' must be defined.")
  }
  
  if ( startdate == enddate ) {
    stop("'startdate' and 'enddate' cannot be equal.")
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
  
  dayCount <- as.integer(difftime(enddate, startdate, units = "days"))
  
  # Choose tickPeriod
  tickPeriod = "days"
  if ( dayCount >= 0 && dayCount <= 7 ) {
    tickPeriod = "days"
  } else if ( dayCount <= 21 ) {
    tickPeriod = "3 days"
  } else if ( dayCount <= 60 ) {
    tickPeriod = "weeks"
  } else {
    tickPeriod = "months"
  }
  
  # Subset based on startdate and enddate
  mon <- monitor_subset(ws_monitor, tlim=c(startdate,enddate))
  
  # Barplot data ---------------------------------------------------------------
  
  dailyData <- monitor_dailyStatistic(mon)$data
  names(dailyData) <- c("datetime", "pm25")
  
  # Add currentNowcast
  if ( !is.null(currentNowcast) ) {
    nowcastDate <- lubridate::floor_date(enddate, "days") + lubridate::ddays(1)
    nextRow <- nrow(dailyData) + 1
    dailyData[nextRow,"datetime"] <- nowcastDate
    dailyData[nextRow,"pm25"] <- currentNowcast
  }
  
  # Color
  dailyData$color = colorPalette(dailyData$pm25)
  
  # Axis limits ----------------------------------------------------------------
  
  if ( ylimStyle == "pwfsl" ) {
    # Well defined y-axis limits for visual stability
    ylo <- 0
    ymax <- max( dailyData$pm25, na.rm = TRUE )
    if ( ymax <= 50 ) {
      yhi <- 50
    } else if ( ymax <= 100 ) {
      yhi <- 100
    } else if ( ymax <= 200 ) {
      yhi <- 200
    } else if ( ymax <= 400 ) {
      yhi <- 400
    } else if ( ymax <= 600 )  {
      yhi <- 600
    } else if ( ymax <= 1000 )  {
      yhi <- 1000
    } else if ( ymax <= 1500 )  {
      yhi <- 1500
    } else {
      yhi <- 1.05 * ymax
    }
  } else {
    # Standard y-axis limits
    ylo <- 0
    yhi <- max(1.05*dailyData$pm25, na.rm = TRUE)
  }
  ylim <- c(ylo, yhi)
  
  # Extend X a little
  xRangeSecs <- as.numeric(difftime(enddate, startdate, timezone, units = "secs"))
  xAxisExtension <- 0.02 * xRangeSecs
  xlim <- c(startdate, enddate)
  
  # AQI Stacked bars -----------------------------------------------------------
  
  if ( showAQIStackedBars ) {
    
    # Get bar width
    width <- 0.01 * xRangeSecs
    right <- startdate - lubridate::dseconds(xAxisExtension) 
    left <- right - lubridate::dseconds(width)
    
    # Modify xlim
    xlim <- c(left, enddate)
    
    # Create data
    aqiStackedBarsData <- data.frame(
      xmin = rep(left, 6),
      xmax = rep(right, 6),
      ymin = c(ylo, AQI$breaks_24[2:6]),
      ymax = c(AQI$breaks_24[2:6], 1e6)
    )
    # Last bar must top out at yhi
    aqiStackedBarsData <- aqiStackedBarsData %>%
      dplyr::filter(.data$ymin < yhi)
    barCount <- nrow(aqiStackedBarsData)
    aqiStackedBarsData$ymax[barCount] <- yhi
    aqiStackedBarsColors <- AQI$colors[1:barCount]
    
  }
  
  
  # Create plot ----------------------------------------------------------------
  
  base_family <- ""
  base_size <- 11 # DELETEME
  half_line <- base_size/2 # DELEMTE
  
  dailyBarplotBase <- ggplot()
  
  if ( showAQIStackedBars ) {
    
    dailyBarplotBase <- dailyBarplotBase + 
      # Add AQI stacked bars
      geom_rect(
        data = aqiStackedBarsData,
        aes(
          xmin = .data$xmin,
          xmax = .data$xmax,
          ymin = .data$ymin,
          ymax = .data$ymax
        ),
        fill = aqiStackedBarsColors)
    
  }
  
  dailyBarplotBase <- dailyBarplotBase + 
    
    # Add daily statistic bars
    geom_bar(
      data = dailyData,
      # See https://www.aj2duncan.com/blog/missing-data-ggplot2-barplots/
      position = position_dodge(preserve = 'single'), # don't drop missing values
      aes(
        x = .data$datetime,
        y = .data$pm25#,
        #fill = .data$color
      ),
      fill = dailyData$color,
      color = borderColor,
      stat = "identity"
    ) +
    
    # Y limits with no extra space below zero
    scale_y_continuous(
      limits = ylim,
      expand = c(0,0)
    ) +
    
    # Add x- and y-axes
    scale_x_datetime(
      limits = xlim,
      expand = c(0,0),
      date_breaks = tickPeriod, 
      date_labels = "%b %d"
    ) +
    ylab("PM2.5 (\u00b5g/m3)") +
    
    # Title
    ggtitle(title)
  
  return(dailyBarplotBase)
  
}





