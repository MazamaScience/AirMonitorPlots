#' @title Create a daily bar plot for a single monitor
#'
#' @description
#' Create a bar plot showing daily average PM2.5 data over a period for the given 
#' monitor. Colored bars represent the PM2.5 readings for each day.
#' 
#' @param mts_monitor \emph{mts_monitor} object containing a single monitor.
#' @param startdate Desired start date (integer or character in Ymd format 
#'        or \code{POSIXct}).
#' @param enddate Desired end date (integer or character in Ymd format
#'        or \code{POSIXct}).
#' @param colorPalette Palette function to convert monitor values into colors.
#' @param ylimStyle Style of y-axis limits. One of \code{auto|pwfsl}.
#' @param aqiStyle AQI style to add AQI color bars, lines and labels.
#' @param borderColor Border color for individual bars.
#' @param borderSize Border size for individual bars.
#' @param currentNowcast Real-time current Nowcast value -- for use in plots 
#' presented in the PWFSL monitoring site.
#' @param currentPrediction Real-time current prediction for today's daily 
#' average -- for use in plots presented in the PWFSL monitoring site.
#' @param dateFormat Format for x-axis dates. Used for \code{date_labels}
#' argument to \code{scale_x_datetime}.
#' @param title Optional title.
#'
#' @return A `ggplot` plot object with a daily bar plot for a single monitor.
#'
#' @importFrom rlang .data
#' @export
#' @examples
#' mts_monitor <- AirMonitor::Carmel_Valley
#' startdate <- "2016-08-05"
#' enddate <- "2016-08-19"
#' dailyBarplotBase(mts_monitor, startdate, enddate)

dailyBarplotBase <- function(mts_monitor,
                             startdate = NULL,
                             enddate = NULL,
                             colorPalette = aqiPalette("aqi"),
                             ylimStyle = "auto",
                             aqiStyle = NULL,
                             borderColor = "black",
                             borderSize = 1.0,
                             currentNowcast = NULL,
                             currentPrediction = NULL,
                             dateFormat = "%b %d",
                             title = "") {
  
  # For debugging --------------------------------------------------------------
  
  if (FALSE) {
    
    # Carmel Valley
    mts_monitor <- AirMonitor::Carmel_Valley
    startdate <- "2016-07-01"
    enddate <- "2016-08-28"
    colorPalette <- aqiPalette("aqi")
    ylimStyle <- "pwfsl"
    aqiStyle <- "bars_lines"
    borderColor <- "black"
    borderSize <- 1
    currentNowcast <- NULL
    currentPrediction <- NULL
    title <- ""
    
  }
  
  # Validate arguments ---------------------------------------------------------
  
  if ( !monitor_isValid(mts_monitor) ) {
    stop("Required parameter 'mts_monitor' is not a valid mts_monitor object.")
  } else if ( monitor_isEmpty(mts_monitor) ) {
    stop("Required parameter 'mts_monitor' is empty.")
  }
  
  if ( !nrow(mts_monitor$meta) == 1 ) {
    stop("Required parameter 'mts_monitor' must contain only one monitor.")
  }
  
  if ( is.null(startdate) && is.null(enddate) ) {
    stop("Required parameters 'startdate' and 'enddate' must be defined.")
  }
  
  if ( startdate == enddate ) {
    stop("'startdate' and 'enddate' cannot be equal.")
  }
  
  # NOTE:  Other parameters are not validated as that is the job of the 
  # NOTE:  calling function.
  
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
  
  # Choose date_breaks
  if ( dayCount >= 0 && dayCount <= 9 ) {
    date_breaks = "1 days"
  } else if ( dayCount <= 21 ) {
    date_breaks = "3 days"
  } else if ( dayCount <= 60 ) {
    date_breaks = "1 weeks"
  } else if ( dayCount <= 120 ) {
    date_breaks = "2 weeks"
  } else {
    date_breaks = "1 months"
  }
  
  # Barplot data ---------------------------------------------------------------
  
  # Subset based on startdate and enddate
  mon <- monitor_subset(mts_monitor,
                        tlim = c(startdate, enddate + lubridate::dhours(23)),
                        timezone = timezone)
  
  dailyData <- monitor_dailyStatistic(mon)$data
  names(dailyData) <- c("datetime", "pm25")
  
  # Add currentNowcast
  if ( !is.null(currentNowcast) ) {
    nowcastDate <- enddate + lubridate::ddays(1)
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
  
  # NOTE:  X-axis must be extended to fit the first and last bars.
  # NOTE:  Then a little bit more for style.
  xRangeSecs <- as.numeric(difftime(enddate, startdate, timezone, units = "secs"))
  marginSecs <- 0.02 * xRangeSecs
  xlo <- startdate - lubridate::ddays(0.5) - lubridate::dseconds(marginSecs)
  xhi <- enddate + lubridate::ddays(0.5) + lubridate::dseconds(marginSecs)
  
  # AQI annotation styling
  if ( !is.null(aqiStyle) ) {
    aqiLineSize <- 0.5     # horizontal lines
    aqiBarWidth <- 0.01    # stacked bars
    if ( stringr::str_detect(aqiStyle, "bars") ) {
      # Set bar width and move xlo to accommodate barWidth and some extra space
      widthSecs <- aqiBarWidth * xRangeSecs
      xlo <- xlo - (3 * lubridate::dseconds(widthSecs))
    }
  }
  
  # Create plot ----------------------------------------------------------------
  
  base_family <- ""
  base_size <- 11 # DELETEME
  half_line <- base_size/2 # DELEMTE
  
  ggplotBase <- ggplot()
  
  if ( !is.null(aqiStyle) ) {
    ggplotBase <- aqiAnnotation(
      ggplotBase, 
      xlo, 
      xhi, 
      ylo, 
      yhi, 
      aqiStyle, 
      aqiBarWidth,
      aqiLineSize
    )
  }
  
  ggplotBase <- ggplotBase + 
    
    # Add daily statistic bars
    geom_bar(
      data = dailyData,
      # See https://www.aj2duncan.com/blog/missing-data-ggplot2-barplots/
      position = position_dodge(preserve = 'single'), # don't drop missing values
      aes(
        x = .data$datetime,
        y = .data$pm25
      ),
      fill = dailyData$color,
      color = borderColor,
      stat = "identity"
    ) +
    
    # Add x- and y-axes
    scale_x_datetime(
      limits = c(xlo,xhi),
      expand = c(0,0),
      date_breaks = date_breaks, 
      date_labels = dateFormat
    ) +
    
    # Y limits with no extra space below zero
    scale_y_continuous(
      limits = c(ylo,yhi),
      expand = c(0,0)
    ) +
    ylab("PM2.5 (\u00b5g/m3)") +
    
    # Title
    ggtitle(title)
  
  # Return ---------------------------------------------------------------------
  
  return(ggplotBase)
  
}





